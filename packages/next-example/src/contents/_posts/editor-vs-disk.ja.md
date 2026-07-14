---
title: "Agentが読んでいるのはバッファか、ディスクか？"
date: 2026-07-13
draft: false
categories:
  - システム
tags:
  - editor
  - agent
  - zed
  - consistency
---

<div class="admonition" data-admonition-type="note">
本稿では、資料調査、Zedのソースコードの読解、文章の推敲、および翻訳にLLMを使用した。
</div>

## メンタルモデル：二つのコピー

エディタを開くと、私たちはファイルを編集しているつもりになる。より正確には、編集しているのは **バッファ** だ。ディスク上のパスは、それとは別のものである。

人間にとって、古典的なデータフローはおおよそ次のようになる。

```
 disk  ----(watch)---->  buffer  ----(UI)---->  human
  ^                         |
  |--------- save ----------|
```

いくつかの暗黙の前提があったため、この仕組みは何十年ものあいだ十分に機能してきた。

- たいていの場合、バッファを編集する人とSaveを押す人は同じである
- conflictが起きたらダイアログを出せる。人間は手を止めて考えられる
- watcherが数十ミリ秒遅れても、人間はほとんど気づかない
- ユーザーにとって「現在のファイル内容」とは、画面に見えているバッファである

agentが入ってくると、図はこう変わる。

```
 human ----edit----> buffer <----agent read/write----
                       ^  \
                       |   \---- save ----\
                       |                   v
 external tools ---- disk <---- shell / fmt / git ----
                       ^
                       |
                    watcher (eventually)
```

agentは同時に三つの役割を担う。バッファを読む観測者、バッファを書く共同編集者、そしてshellを実行してディスクを変更する「外部プロセス」である。さらに厄介なことに、agentはしばしば **自分で外部変更を作り、その直後にreadする**。システムが自分自身とraceすることになる。

ここでは具体例として[Zedエディタ](https://zed.dev/)を使う。
Zedは[ACP（Agent Client Protocol）](https://agentclientprotocol.com/)を通じて、CodexやClaude Codeのような外部coding agentを接続する。

ローカル接続では、エディタがclientとしてagentのsubprocessを起動し、両者は`stdio`上の`JSON-RPC`で通信する。requestは双方向だ。エディタはagentへpromptを送り、agentは逆にエディタへファイルのread/write、terminalの作成、ユーザーへのpermission要求を依頼できる。

本稿に直接関係するのは、agentからエディタへ送られる`fs/read_text_file`と`fs/write_text_file`である。名前だけを見るとpath APIのようだが、Zedはまず`project.open_buffer`を呼ぶ。readはバッファのsnapshotを返し、writeはsnapshotとの差分を計算し、anchorを使ってバッファへ適用したあと、最後に`save_buffer`する。つまり、Zedの選択は次のほうに近い。

> **セッション内のバッファオブジェクト**をread/writeし、writeの最後に`save_buffer`する。

毎回パスに対してversion付きのPOSIX read/writeを行うわけではない。どちらの設計にも一貫した説明はできる。問題は、agentが`echo`や`cargo fmt`、code generatorも実行することだ。それらが触るのはディスクである。

shellがディスクを書き換えたあと、Zedがバッファを更新するにはfile watcherを経由する必要がある。バッファに未保存の編集があれば、その更新で単純に上書きすることもできない。見かけ上は同じ二つの「ファイル」が、ここから分かれ始める。

二つのACP file requestについて、Zedの契約は明確だ。read/writeするのはバッファである。ただしagentはterminalでshellを実行し、これらのrequestを迂回してディスクを直接read/writeできる。一つのtool loopが二つの経路を交互に使うと、バッファとディスクの同期過程が表面に現れる。以下の四つのケースは、すべてこの境界で起きる。

## 四つの具体的なケース

以下の四つの図は、どれも「古いファイルを読んだ」話に見える。しかし実際には、通るcode pathが異なる。まず、それぞれの場面でZedがどう振る舞うかを整理する。

1. shellはディスクを書き換えたが、Zedはまだwatcher eventを処理していない。これはtiming windowであり、eventの処理後、cleanなバッファはリロードされる。
2. watcher eventは処理されたが、mtimeもsizeも変わっていない。Zedはdisk stateが同じだと判断し、リロードしない。
3. バッファに未保存の編集がある。Zedは意図的にバッファを保持し、外部conflictを記録する。ACP readはそれでもバッファのテキストだけをagentへ返す。
4. ACP writeが変更するのはZedのCRDTバッファなので、人間の未保存編集とmergeできる。shellによるディスクの変更は、この共同編集経路を通らない。

これらは互いに独立している。以下のsequence diagramでは、縦線がparticipant、矢印の横が観測可能なaction、`???`が挙動の落とし穴を示す。

### 1. 外部writeは完了したが、watcherがまだsettleしていない

agentのtool loopは、しばしば「shellでディスクを変更し、直後にACP readを送る」という動きをする。その間にevent loopが処理を終えるのを待つとは限らない。readがopen bufferのsnapshotを取るだけで、まだ`file_updated`が起きていなければ、ディスクがすでに`v2`でも二度目のreadは古いテキストを返す。

```
  agent              buffer              disk               watcher
    |                   |                  |                    |
    |-- read_text ----->|                  |                    |
    |   open / snapshot |                  |                    |
    |<-- "v1" ----------|                  |                    |
    |                   |                  |                    |
    |-- shell: echo v2 > foo ------------->|                    |
    |                   |                  |-- inotify (soon) ->|
    |                   |                  |                    |
    |-- read_text ----->|                  |                    |
    |   (no wait)       |                  |                    |
    |   snapshot again  |  still "v1"      |  content = "v2"    |  event maybe
    |<-- ??? "v1" ------|                  |                    |  not processed
    |                   |                  |                    |
    |                   |<-- file_updated -|<- process later ---|
    |                   |   (too late for that read)            |
```

「eventがまだ伝播中」の場合もあれば、「eventはfdに届いているが、アプリケーション側がまだリロードしていない」場合もある。agentから観測できる結果は同じで、staleな内容を読む。

Zedでは、このreadが新しいディスク内容を見られるかどうかは、watcherの通知がreadまでにバッファへ反映されたかに依存する。通常のprojectでは、この経路はたいてい速い。次のreadまでにmodel callを一度挟めば、バッファは多くの場合すでにリロードされている。shellとreadが隣接していると、このtiming windowに入りやすい。

### 2. mtimeもsizeも変わらない

Zedはfile eventを受け取っても、すぐには新旧の内容を比較しない。`Buffer::file_updated`が比較するのは二つの`DiskState`であり、ディスク上に存在するファイルの場合、そのstateに含まれるのはmtimeとsizeだけである。したがって、リロードするかどうかは実質的に次のpredicateで決まる。

```
changed = (mtime' != mtime) || (size' != size)
```

長さを変えず、mtimeも維持したままin-place rewriteすると、`old_state == new_state`になる。つまり、**watcherはsettleし、`file_updated`も実行済みなのに、`ReloadNeeded`は送られない**。これはlivenessではなくsafetyの問題である。

```
  agent              buffer              disk               file_updated
    |                   |                  |                    |
    |-- read ---------->|                  |                    |
    |<-- "fn foo ( ) {" |  size=12         |  mtime=T0          |
    |                   |  saved meta=T0   |                    |
    |                   |                  |                    |
    |            external rewrite, same size, mtime stays T0    |
    |                   |                  |-- "fn foo() {  " ->|
    |                   |                  |   size=12, mtime=T0|
    |                   |                  |                    |
    |              watcher fully settles                        |
    |                   |                  |                    |
    |                   |<-- file_updated -|--------------------|
    |                   |   old (T0,12)    |                    |
    |                   |   new (T0,12)    |                    |
    |                   |   equal? yes ----|--> no ReloadNeeded |
    |                   |  content still   |  content already   |
    |                   |  "fn foo ( ) {"  |  "fn foo() {  "    |
    |                   |                  |                    |
    |-- read ---------->|                  |                    |
    |<-- ??? stale -----|                  |                    |
```

現在のext4はnanosecond精度のmtimeを持つため、普段のformatterが「同じmtime」に当たることは考えにくい。それでもpredicateが信じているのはmetadataだけだ。metadataが嘘をつけば、event loopが完了してもシステムは自分が正しいと思い続ける。

### 3. dirtyなバッファは自動リロードされない

未保存の編集を保持するのは、Zedの意図した挙動である。人間がバッファへ`HUMAN`を挿入し、まだsaveしていないとする。そのあとagentがterminalから`echo 'DISK_V2' > file`を実行する。watcherはディスクの変更をZedへ知らせるが、バッファがdirtyなので、Zedは`DISK_V2`で上書きせず、代わりにconflict stateへ入る。

ここでagentが`fs/read_text_file`を送ると、返るのは未保存の`HUMAN\nv1`のままだ。直前のshell writeを開始したのはagent自身なのに、read responseにはテキストしかなくconflict stateがないため、ディスクが`DISK_V2`になったことを判別できない。

```
  human              buffer              disk               agent
    |                   |                  |                  |
    |-- edit ---------->|                  |                  |
    |   insert HUMAN    |  dirty=true      |  "v1"            |
    |                   |  text=HUMAN+v1   |                  |
    |                   |                  |                  |
    |                   |<-- read_text --------------------|
    |                   |--- "HUMAN\nv1" ------------------>|
    |                   |                  |                  |
    |         agent: echo DISK_V2 > file   |                  |
    |                   |                  |<-- DISK_V2 ------|
    |                   |                  |                  |
    |              watcher settles         |                  |
    |                   |                  |                  |
    |                   |  was_dirty? yes  |                  |
    |                   |  => no reload    |                  |
    |                   |  has_conflict=1  |  disk=DISK_V2    |
    |                   |  text still      |                  |
    |                   |  HUMAN+v1        |                  |
    |                   |                  |                  |
    |                   |<-- read_text --------------------|
    |                   |--- ??? "HUMAN\nv1" -------------->|
    |                   |   (no conflict in API)              |
```

errorなら少なくとも後続処理を止められる。古い文字列を黙って返すと、agentは間違った状態を前提に推論を続けてしまう。

### 4. writeが二種類の並行変更に出会う

agentのreadからwriteまでの間に、人間がファイルを編集する場合もあれば、shellが変更する場合もある。前者はバッファで起こり、後者はディスクで起こる。Zedは両者を異なる方法で扱う。

**4a. 人間とagentがともにバッファを編集する**

```
  human              buffer                 agent
    |                   |                      |
    |                   |  base = one\ntwo\nthree\n
    |                   |                      |
    |                   |<-- read -------------|
    |                   |--- base ------------>|
    |                   |                      |
    |-- edit head ----->|                      |
    |   ZERO\n + base   |  dirty               |
    |                   |                      |
    |                   |<-- write (append four)
    |                   |   diff vs snapshot   |
    |                   |   anchor apply       |
    |                   |  => ZERO\none\n..\nfour\n
    |                   |                      |
    |                   |--- Ok / merge ------>|
```

このケースではZedに何の問題もない。ZedのバッファはCRDTであり、ACP writeもファイルを直接上書きしない。前回のreadで得たsnapshotとの差分を計算し、anchorを使って現在のバッファへeditを適用する。人間の`ZERO`とagentの`four`は別の場所を変更しているので、両方が残る。

**4b. agentのread後にshellがディスクを書き換える**

一行だけのファイルを考える。agentは`mode = "dev"`を読み、`mode = "test"`へ変えようとする。そのwriteより前に、別のshellがディスクを`mode = "prod"`へ変更していたとする。

```
  agent                 buffer                 disk                  shell
    |                       |                    |                      |
    |-- read ------------->|                    |                      |
    |<-- mode = "dev" -----|                    |  mode = "dev"        |
    |                       |                    |                      |
    |                       |                    |<-- mode = "prod" ----|
    |                       |   reload or not    |                      |
    |                       |                    |                      |
    |-- write mode="test" >|                    |                      |
    |   based on "dev"      |-- edit + save --->|                      |
    |                       |                    |  mode = "test"       |
    |<-- Ok ----------------|                    |  "prod" is gone      |
```

watcherが間に合ってリロードしたかどうかにかかわらず、このwriteは`mode = "prod"`を上書きする可能性がある。agentの判断は古い値`dev`に基づいているが、ACP requestには「どのversionを読んだか」が含まれない。Zedにできるのはeditを適用し、saveし、成功を返すことだけだ。

interfaceが「前回のread以降、ディスクが変わっていない場合だけwriteを許可する」と要求するなら、readはrevisionを返し、writeはそれを送り返して比較する必要がある。一致しなければwriteを拒否し、agentにreadをやり直させる。ここでいうcompare-and-swapとはそれである。これは選択可能なinterface constraintの一つであり、4aのCRDT mergeが自動的に解決できる問題ではない。

---

四つのうち、1番目が最も「典型的なtiming race」に見える。そこで自然に出てくる疑問は、Linuxのinotifyはそんなに頻繁に間に合わないのか、というものだ。

## 小さな実験：ローカルext4でwatcherはどれほど早く発火するか

執筆に使ったマシンでは、`/tmp`と`/home`の両方がext4である。raw `inotify`（`IN_MODIFY` | `IN_CLOSE_WRITE`）で測るのは **カーネルからのevent delivery** であり、Zedのworktreeパイプライン全体ではない。しかし、この層がすでに非常に速いなら、staleの主因を「OS notificationが遅い」と考えるべきではない。

| 測定 | 結果（おおよその桁） |
|------|----------------------|
| `close`が返ってからinotifyがreadableになるまで | p50 **~10 µs** |
| `close`直後のnonblocking `select`がready | **~100%** |
| `bash -c 'echo > f'`が返った時点でeventがfd上にある | **ほぼ常に**（shell自体は~1–2 ms） |
| write直後のpath readで新しい内容が見えるか | **はい** |

同時に、アプリケーション側が「open bufferだけを返し、このturnではwatch eventを処理しない」経路なら、inotify eventがすでに届いているかどうかに関係なく、ディスクに対して **安定してstale** になる。eventをdrainしてリロードすれば、staleは0になる。

実際のagent timelineと比べてみる。一回のLLM tool round tripは通常、数百ミリ秒から数秒かかり、inotifyよりはるかに遅い。複数turnにまたがって「shellがファイルを変更する -> modelが少し考える -> もう一度readする」なら、その間隔でwatcherがsettleすることが多い。影響を受けやすいのは、**同じtool sequence内でshellとreadが隣接する場合**、または **readが返る前にディスクとの整合をまったく取らない場合**である。

したがって、「カーネル + ローカルディスク」について言えば、

> **watcherが「発火に間に合わない」可能性は低い。**  
> agentがそれでも古い内容を読むなら、バッファを読んでいるか、すでに届いたeventをアプリケーションがまだリロードへ変換していない可能性が高い。

## おまけ：ファイルが多すぎると「watcherが発火しない」

上の実験が測っているのは、**inotify fdにイベントがあるか**である。エディタ内部には、さらに長いパイプラインがある。

```
inotify -> worktree process_events -> UpdatedEntries -> file_updated -> ReloadNeeded -> open/read disk
```

途中のどこかでeventが捨てられれば、UIにもagentにも「watcherが鳴らなかった」ように見える。

以前、同じプロセスで二つのZed windowを開いていた。一つはほぼ空のminimal directory、もう一つはrootに巨大な`node_modules`を持つmonorepoで、さらにYarn workspaceによる次のsymlinkがあった。

```text
node_modules/next-example -> ../packages/next-example
```

比較結果は次のとおりだった。

- minimal window、およびmonorepoの **root / 小さなpackage** にあるopen bufferは、外部からディスクを変更するとすぐリロードした
- monorepoで **workspaceから`node_modules`へlinkされたpackage** にあるopen bufferでは、inotifyはファイル名を返したのに **`file_updated`が実行されず**、ディスクから再読み込みもされなかった。バッファは古い内容のままだった

経路を追うと、正常なpathはおおよそ「inotify event -> worktree entry更新 -> `file_updated` -> ディスク上のファイルを開き直す」となる。異常なpathは途中で止まる。カーネル側にはeventが届いているのにバッファがリロードされない。Zedの`worktree::process_events`には、この現象によく合うfilterがある。

```rust
// 親directoryはEntryKind::Dirでなければならない。UnloadedDirまたは欠落ならeventを捨てる。
if !parent_dir_is_loaded {
    log::debug!("filtering event {relative_path:?} within unloaded directory");
    // entryを更新せずにdropする。
}
```

この種のlogはdefault設定ではほとんど見えないため、体感としては単に「watcherが壊れた」となる。より正確には次のように言える。

> **カーネルwatchは発火した。file treeが巨大だったりscan / ignore stateが乱れていたりすると、worktreeがアプリケーション層でeventを捨てることがある。**

発火条件には、ignoreされていない巨大なdependency tree、workspace symlinkによる二重path、長時間のsessionでsubtreeが`UnloadedDir`になってもinotify watchが残っている、といった事情が重なりやすい。minimal projectではほとんど起きない。monorepoのignoreがrootの`node_modules`を漏らすと、この状態に入りやすくなる。

これがおまけであり、同時に注意点でもある。**fd上にeventがあること**と、**アプリケーション側でリロードされること**は同じではない。

## 結論

Zedの選択に戻ると、`fs/read_text_file`と`fs/write_text_file`がread/writeするのはバッファである。これによって人間の未保存編集が消えず、人間とagentの非重複変更はCRDTでmergeできる。同じagentがterminalを開いてディスクを直接変更すると、システムにはもう一つの経路が増える。ディスク上の変更がバッファへ届くにはwatcherが必要になる。

ローカルext4での実験により、最初のケースが起きる範囲はかなり狭いとわかった。`echo`プロセスが返る時点で、inotify eventはほぼ常にreadableになっている。通常のprojectなら、model callをもう一度挟む時間でZedはたいていリロードできる。それでもstaleなら、次に調べるべきはworktreeがeventを処理したか、途中で捨てたかである。shellとreadが隣接するときにだけ、短いtiming windowが第一候補になる。

四つのケースから、四つのopen questionが残る。

1. `fs/read_text_file`の契約はbuffer snapshotであるべきか、それともディスク上の現在内容であるべきか。バッファを選ぶなら、callerはディスクとの明示的な同期やディスクの直接readをどう要求するのか。
2. file eventを受け取ったのにmtimeとsizeが同じなら、metadataを信じるべきか、内容やhashをさらに比較するべきか。後者のcostは誰が負担するのか。
3. conflictをread responseに含めるべきか。テキストだけを返す、revision / stateを添える、errorにする、という選択肢があり、agentが観測できる情報はそれぞれ異なる。
4. writeがbaseの変更を検出したとき、mergeを試みるべきか、last-write-winsを許すべきか、compare-and-swapで拒否するべきか。バッファ内の変更とdisk bypassに同じruleを使うべきか。

この問いをもう一段さかのぼると、エディタのSSOTはどちらなのか、という問題になる。この境界は[1972年のUnix Second Edition `ed` manual](https://www.tuhs.org/Archive/Distributions/Research/1972_stuff/unix_2nd_edition_manual.pdf)ですでに明記されていた。`ed`はファイルのcopyを編集し、変更はバッファ内で行われ、ユーザーが`w`を実行したときだけ元のファイルへ書き戻された。さらに興味深いことに、そのバッファ自体もtemporary fileに置かれていた。バッファ / ディスクは、最初から単なるRAM / diskという物理的な違いではない。「編集中のworking copy」と「名前を持ち、他のprogramが利用するpersistent file」の違いだった。editing session内ではバッファが現在状態であり、compilerやshellに見えるのはnamed fileだけである。

その後のfull-screen editorはworking copyにcursor、undo、selection、multiple windowを追加したが、基本的な境界は変わっていない。ProseMirrorはbuffer-only側に近く、persistenceをhostへ任せる。`sed -i`のようなpath transformationはdisk-only側に近い。現代のcode editorには両方が必要だ。バッファは未完成の編集を保持し、ディスクはcompiler、formatter、Git、shellに使われる。

[Claude Code](https://docs.anthropic.com/en/docs/claude-code/getting-started)と[Codex CLI](https://help.openai.com/en/articles/11096431)は、そもそもエディタを経由しなくても動作できる。working directoryへ入り、pathからファイルを読み、ディスクへ直接書き戻し、shellでformatter、compiler、testを実行する。agentにはcursorもselectionも、画面上の未保存テキストも必要ない。taskのあいだagentがworktreeを独占するなら、ディスクはすべてのtoolが理解できるhandoff surfaceになる。したがって、本稿で述べたconflictは、無人のagentic loopでは発生しない。

しかし、人間とagentのco-workが短期的になくなることはないだろう。本稿で扱った問題は、今後一、二年は **long-tail** のユーザーを繰り返し悩ませると私は予想している。watcherが動かず、agentが意図と違うファイルを編集したときには、この記事をもう一度開いてみてほしい。

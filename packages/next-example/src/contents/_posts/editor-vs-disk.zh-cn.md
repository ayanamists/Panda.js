---
title: "Agent 读到的是 buffer，还是 disk？"
date: 2026-07-13
draft: false
categories:
  - 系统
tags:
  - editor
  - agent
  - zed
  - consistency
---

<div class="admonition" data-admonition-type="note">
本文在资料检索、Zed 源码阅读和文字修改中使用了 LLM。
</div>

## 心智模型：两份文件

打开一个编辑器，你以为自己在编辑文件。更准确地说，你在编辑一份 **buffer**。磁盘上的那个 path，是另一件事。

对人来说，经典数据流大概是这样：

```
 disk  ----(watch)---->  buffer  ----(UI)---->  human
  ^                         |
  |--------- save ----------|
```

几条默认约定让这套东西几十年都够用：

- 多数时候，写 buffer 和点 Save 的是同一个人
- 冲突了可以弹窗；人可以停下来想
- watcher 晚几十毫秒，人几乎感觉不到
- 对用户而言，“当前文件内容”就是屏幕上那份 buffer

Agent 进来以后，图会变成这样：

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

Agent 同时扮演三种角色：读 buffer 的观察者、写 buffer 的协作者、以及会跑 shell 去改 disk 的“外部进程”。更麻烦的是，它还经常 **自己制造外部修改，再立刻 read**——系统在和自己 race。

我们这里用 [Zed 编辑器](https://zed.dev/) 作为实际例子。
Zed 通过 [ACP（Agent Client Protocol）](https://agentclientprotocol.com/)接入 Codex、Claude Code 一类外部 coding agent。

在本地接入里，编辑器作为 client 启动 agent 子进程，两边通过 `stdio` 上的 `JSON-RPC` 通信。这里的请求是双向的：编辑器把 prompt 发给 agent，agent 也可以反过来请求编辑器读文件、写文件、开 terminal 或向用户申请权限。

和本文直接相关的是两个 agent 发给编辑器的文件系统请求：`fs/read_text_file` 和 `fs/write_text_file`。名字读起来像 path API，不过 Zed 实际会先 `project.open_buffer`：read 返回 buffer snapshot 里的文本，write 根据 snapshot 计算 diff，通过 anchor 修改 buffer，最后 `save_buffer`。也就是说，Zed 的选择更接近：

> 读写 **会话里的 buffer 对象**，write 末尾再 `save_buffer`。

而不是每次对 path 做一次带版本的 POSIX 读写。两种选择都能自圆其说；别扭只在于 agent 还会 `echo`、`cargo fmt`、改生成物——那些动的是 disk。

shell 改完 disk 后，Zed 还要经过文件 watcher 才能更新 buffer；如果 buffer 里有未保存的编辑，这次更新又不能直接覆盖。两个看似等价的“文件”从这里开始分叉。

Zed 对两个 ACP 文件请求的约定很明确：它们读写 buffer。只是 agent 还可以从 terminal 里运行 shell，绕过这两个请求直接读写 disk。同一个 tool loop 里一旦交错使用两条通路，buffer 与 disk 之间的同步过程就会露出来。下面四个场景，都发生在这里。

## 四个具体场景

下面四张图看起来都在说“读到了旧文件”，实际走到的是不同代码分支。先把 Zed 在每个场景里的表现写清楚：

1. shell 已经改完 disk，watcher 事件还没被 Zed 处理：这是一个 timing window；事件处理完，干净的 buffer 会 reload。
2. watcher 事件已经处理，但文件的 mtime 和 size 都没变：Zed 认为 disk state 没变，不会 reload。
3. buffer 里有未保存编辑：Zed 有意保留 buffer，只记录外部冲突；ACP read 仍然只把 buffer 文本返回给 agent。
4. agent 通过 ACP write 修改的是 Zed 的 CRDT buffer，能与人的未保存编辑合并；shell 对 disk 的修改不走这条协作通道。

它们彼此独立。下面每张时序图里，竖线是参与者，箭头旁是可观察动作；`???` 标出容易踩空的地方。

### 1. 外部已写完，watcher 还没 settle

agent 的工具循环经常是：shell 改盘 → 立刻再发起 ACP read，中间往往不等事件循环跑干。若 read 只拍 open buffer 的 snapshot，而 `file_updated` 尚未发生，第二次 read 仍是旧文本——哪怕 disk 上已经是 `v2`。

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

原因可以是 “事件还在路上”，也可以是 “事件已到 fd，业务路径还没 reload”。对 agent 而言，可观察结果一样——读到了 stale。

在 Zed 里，这次 read 能否看到新的 disk 内容，取决于 watcher 有没有及时把变化送到 buffer。正常项目中这条链路通常很快，隔着一次模型调用再 read，buffer 多半已经 reload；shell 和 read 紧挨着发生时，才容易落进上图的窗口。

### 2. mtime 与 size 都没变

Zed 收到文件事件后，并不会立刻比较新旧内容。`Buffer::file_updated` 比较的是两次 `DiskState`；对磁盘上存在的文件，这个状态只包含 mtime 和 size。于是，是否 reload 实际取决于：

```
changed = (mtime' != mtime) || (size' != size)
```

同长度、且 mtime 被保持不变的原地改写，会让 `old_state == new_state`。于是 **watcher 已经 settle，`file_updated` 也跑了，仍然不发 `ReloadNeeded`**。这是 safety 问题，不是 liveness 问题。

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

现代 ext4 纳秒 mtime 下，日常 fmt 很难撞上“同 mtime”；但谓词本身只看 metadata。一旦 metadata 撒谎，事件循环跑完了系统仍认为自己是对的。

### 3. dirty buffer 不自动 reload

保留未保存编辑是 Zed 有意为之。假设人先在 buffer 里插入 `HUMAN`，还没有保存；agent 随后从 terminal 执行 `echo 'DISK_V2' > file`。watcher 会告诉 Zed disk 变了，但因为 buffer 是 dirty 的，Zed 不会用 `DISK_V2` 覆盖它，只会进入 conflict 状态。

这时 agent 再发起 `fs/read_text_file`，拿到的仍是未保存的 `HUMAN\nv1`。刚才那次 shell 写入是 agent 自己发起的，它却无法从 read 的返回值里看出 disk 已经变成 `DISK_V2`，因为响应里只有文本，没有 conflict 状态。

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

报错至少还能拦住后续步骤；静默返回旧字符串，后面的推理会接着错下去。

### 4. write 遇到两种并发修改

agent 从 read 到 write 之间，文件可能被人改，也可能被 shell 改。前者发生在 buffer 里，后者发生在 disk 上，Zed 处理它们的方式并不相同。

**4a. 人和 agent 都改 buffer**

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

这个场景在 Zed 里没有问题。Zed 的 buffer 是 CRDT；ACP write 也不直接覆盖文件，而是根据上次 read 的 snapshot 计算 text diff，再用 anchor 把 edit 应用到当前 buffer。人的 `ZERO` 和 agent 的 `four` 没有改到同一处，两边都会保留下来。

**4b. agent read 之后，disk 被 shell 改写**

换一个只有一行的文件。agent 读到 `mode = "dev"`，准备把它改成 `mode = "test"`。在 write 发生前，另一个 shell 已经把 disk 改成了 `mode = "prod"`：

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

无论 watcher 是否来得及 reload，最终的 `mode = "prod"` 都可能被这次 write 覆盖。关键在于 agent 的决定基于旧值 `dev`，ACP 请求里却没有带上“我读到的是哪个版本”。Zed 只能应用 edit、保存，然后返回成功。

如果接口要求“disk 自上次 read 后没变才允许 write”，read 就要返回一个 revision，write 再把它带回来比较；不相等时拒绝写入，要求 agent 重新读取。这就是这里所说的 compare-and-swap。它是一种可以选择的接口约束，不是 4a 那种 CRDT merge 能自动解决的问题。

---

这四类里，第 1 类看起来最像“经典 timing race”。一个很自然的反应是：Linux 上 inotify 是不是经常来不及？

## 小实验：本机 ext4 上，watcher 有多容易触发？

写作时的机器：`/tmp` 与 `/home` 都是 ext4。用 raw `inotify`（`IN_MODIFY` | `IN_CLOSE_WRITE`）测的是 **内核交付**，不是 Zed 整条 worktree 管道；但若这一层已经极快，就不宜把 stale 主要归咎于“OS 通知太慢”。

| 测量 | 结果（数量级） |
|------|----------------|
| close 返回后到 inotify 可读 | p50 **~10 µs** |
| close 后立刻非阻塞 `select` 已就绪 | **~100%** |
| `bash -c 'echo > f'` 返回时事件已在 fd 上 | **几乎总是**（shell 本身 ~1–2 ms） |
| write 后 path read 是否立刻看到新内容 | **是** |

同一时刻，若业务路径是“只返回 open buffer、本轮不处理 watch”，则相对 disk **稳定 stale**——与 inotify 是否已到无关。drain 事件并 reload 之后，stale 变为 0。

和 agent 真实时间线比：一次 LLM tool 往返通常是百毫秒到数秒，远大于 inotify。多轮对话里 “shell 改文件 → 想一会儿 → 再 read”，中间空档通常够 watcher settle。更敏感的是：**同一轮工具里紧挨着 shell 与 read**，或 **read 根本不在返回前对齐 disk**。

所以就“内核 + 本地盘”而言：

> **watcher 不太可能“来不及触发”。**  
> 若 agent 仍读到旧内容，更常见的是：读的是 buffer，或应用层还没把已到达的事件变成 reload。

## 彩蛋：文件太多时，“watcher 好像不触发”

上面实验说的是 **inotify fd 上有没有事件**。编辑器里还有一截更长的管道：

```
inotify → worktree process_events → UpdatedEntries → file_updated → ReloadNeeded → open/read disk
```

中间任何一层丢掉事件，UI 和 agent 看到的都像“watcher 没响”。

有一次，我在本机同时开着两个 Zed 窗口（同一进程）：一个 minimal 目录几乎空；一个 monorepo，根上挂着巨大的 `node_modules`，外加 yarn workspace 的

```text
node_modules/next-example -> ../packages/next-example
```

对照实验结果是：

- minimal 窗口、以及 monorepo **根目录 / 小包** 下的 open buffer：外部改盘后很快 reload
- monorepo 里 **某个被 workspace 链进 `node_modules` 的包** 下的 open buffer：inotify 仍能读到文件名，但 **`file_updated` 不走**，也不会重新从磁盘加载；buffer 一直停在旧内容

顺着链路看，好路径大致是：inotify 读到事件 → worktree 更新条目 → `file_updated` → 重新打开磁盘文件。坏路径停在中间：内核侧事件到了，buffer 却不 reload。源码里有一处和现象很贴的过滤（`worktree` 的 `process_events`）：

```rust
// 父目录必须是 EntryKind::Dir；UnloadedDir / 缺失则丢弃
if !parent_dir_is_loaded {
    log::debug!("filtering event {relative_path:?} within unloaded directory");
    // drop — 不继续更新
}
```

这类日志默认几乎看不见，所以体感就是“watcher 坏了”。更准确的说法是：

> **内核 watch 触发了；worktree 在“文件太多 / 扫描与忽略状态很脏”时，可能把事件在应用层丢掉。**

触发条件往往和这些叠在一起：未 ignore 的巨大依赖树、workspace symlink 双路径、长会话里子树变成 `UnloadedDir` 却仍挂着 inotify。minimal 工程几乎遇不到；一旦 monorepo 的 ignore 漏了顶层 `node_modules`，就比较容易踩中。

这是彩蛋，也是提醒：要分清 **fd 上有没有事件** 和 **业务有没有 reload**。两者不是一回事。

## 结论

回到 Zed 的选择，`fs/read_text_file` 和 `fs/write_text_file` 读写的是 buffer。这保住了人的未保存编辑，也让人与 agent 的非重叠修改可以通过 CRDT 合并。同一个 agent 打开 terminal、直接修改 disk 之后，系统里又多出了一条通路；disk 上的变化要靠 watcher 才能回到 buffer。

本机 ext4 上的实验把第一个场景缩小到了很窄的范围。`echo` 进程返回时，inotify 事件几乎总是已经可以读取；正常项目再隔一次模型调用，Zed 通常有足够时间 reload。此时仍然 stale，就该继续检查 worktree 有没有处理或丢掉事件。只有 shell 与 read 紧挨着发生时，那个短暂的 timing window 才更值得怀疑。

这四个场景最后留下了四个 open question：

1. `fs/read_text_file` 的契约应该是 buffer snapshot，还是 disk 上的当前内容？如果选择 buffer，调用方要怎样显式要求对齐或读取 disk？
2. 收到文件事件、mtime 和 size 却完全相同时，应该相信 metadata，还是继续比较内容或 hash？后者的开销由谁承担？
3. conflict 要不要进入 read 的响应？接口可以只返回文本，也可以附带 revision / 状态，或者直接报错；agent 能观察到的信息并不相同。
4. write 发现 base 已经变化时，应该尝试 merge、允许 last-write-wins，还是按 compare-and-swap 拒绝？buffer 内修改与 disk 旁路修改是否应该使用同一套规则？

顺着这四个问题再往前追，编辑器的 SSOT 到底是哪一份？这条分界至少在 [1972 年 Unix 第二版的 `ed` 手册](https://www.tuhs.org/Archive/Distributions/Research/1972_stuff/unix_2nd_edition_manual.pdf)里就已经写得很清楚：`ed` 编辑文件的一份 copy，修改只发生在 buffer 中，直到用户执行 `w` 才写回原文件。更有意思的是，那份 buffer 本身也放在临时文件里。可见这里的 buffer / disk 从一开始就不是 RAM / disk 的物理区别，而是“正在修改的工作副本”和“有名字、供其他程序使用的持久文件”。在编辑会话里，buffer 是当前状态；对编译器和 shell，命名文件才是它们能看见的状态。

后来的全屏编辑器在工作副本上继续加入 cursor、undo、selection 和多窗口，基本边界没有变。ProseMirror 走得更靠 buffer 一端，把持久化交给宿主；`sed -i` 这类 path transformation 则更靠 disk 一端。现代代码编辑器两边都要：buffer 承载尚未完成的编辑，disk 服务编译器、formatter、Git 和 shell。

[Claude Code](https://docs.anthropic.com/en/docs/claude-code/getting-started) 和 [Codex CLI](https://help.openai.com/en/articles/11096431) 本来就可以不经过编辑器：进入一个工作目录，从 path 读文件，直接写回 disk，再调用 shell 跑 formatter、编译器和测试。agent 不需要光标、selection 或屏幕上尚未保存的那份文本；只要任务期间由它独占 worktree，disk 就足以充当所有工具都认识的交接面。所以，本文所描述的冲突在 agentic loop 之类的无人值守工作中，并不存在。

然而，人类和 agent co-work 的需求，短期内不会消失。我预计本文提到的问题，一两年内还会反复折磨 **长尾** 用户 -- 当 watcher 不工作，agent 改不对文件的时候，不妨回来翻翻吧！

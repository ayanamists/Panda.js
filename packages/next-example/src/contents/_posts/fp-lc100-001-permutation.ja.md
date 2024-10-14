---
title: "関数型 Leetcode 百題 -- 順列"
date: 2024-04-24T19:38:38+08:00
draft: false
categories:
    - アルゴリズム
tags:
    - Haskell
    - 関数型アルゴリズム
---

<div class="admonition" data-admonition-type="warning">
この記事は AI による中国語からの翻訳です。誤りが含まれている可能性がありますので、ご了承ください。
</div>

## 序章

アルゴリズムの問題は、多くのプログラマにとっての悪夢の一つです。アルゴリズム自体は確かに興味深いものですが、問題を解けない焦燥感や面接で答えられない恥ずかしさは誰もが好まないものです。

アルゴリズムの問題への対処法としては、**問題を解く** という伝統的な方法があります。問題を解くとは、大量の問題を解いてアルゴリズムを学ぶことです。多くの人々は、数学の学習もアルゴリズムの学習も、大量の練習が必要だと信じています。

私もその理論に賛成です。ノイマンの格言は、このブログのトップページに掲げられています：

> 若者よ、数学では物事を理解するのではなく、慣れるだけだ。

直感的な理解、直感的な経験は、無比の価値を持つ財産です。練習はその貴重な財産を得る最も確実な手段です。要するに、アルゴリズムを学ぶためには、問題を解き、練習しなければなりません。

しかし、私はある種のアルゴリズム学習の常道を極度に嫌っています。それは、ある問題を解けない、10分間考えてからも手がかりが見つからない、そこで答えを探すという道です。あなたは答えを "理解" したり、答えを覚えておいたり、次に同じ問題が出てきたときに当てはめたりするかもしれません。あなたが見た答えが十分に多いなら、あなたが "当てはめる" ことができる問題も十分に多いでしょう。

なぜ私はその学習法が好きではないのでしょうか？私はそれほど頭が良くないのかもしれません。私は他人が提供する答えを "理解" するのが難しいです。あまりにも多くの人々は、最終的な効率的なアルゴリズムだけを教えてくれ、直感の形成の過程やアルゴリズムの証明を無視します。彼らの答えから、彼らが問題に対する深い理解を感じさせられず、またアルゴリズム自体の美しさを感じさせられません。

私たちは、"天才" に向けた教育を通じてしかアルゴリズムを学ぶことができないのでしょうか？

いいえ。世界には、純粋な関数型言語を使い、等式推導の方法でプログラムを設計しようとする人々がいます。彼らもまた直感の価値を大切にしています。彼らは、明確で効率の悪いプログラムから出発し、最終的な効率的なアルゴリズムへと一歩ずつ進みます。その過程で、アルゴリズムの美しさ、アルゴリズムの洞察が鮮やかに浮かび上がります。故リチャード・バード（Richard Bird）や台湾の穆 信成 先生は、その手法の先駆者です。彼らの論文と本の中で、私は真の優しさを感じました。

私は、等式推導はしばしば非常に困難であるかもしれませんが、アルゴリズムの non-trivial な部分が何であるか、アルゴリズムの直感が何であるかを明確に説明することは、それほど困難ではないかもしれません、と考えています。

このシリーズの記事は、私の試みです。これらの記事では、私は純粋な関数型プログラミングを使って Leetcode をどのように解くかを説明します。なぜ Leetcode を選んだのか、より難易度の高い Codeforces を選ばなかったのかというと、難易度が高すぎることは必ずしも良いことではないと考えているからです。難易度の高い問題は、しばしば複数のアルゴリズムが必要となる可能性があり、私たちは各記事で特定のアルゴリズムに焦点を当てたいと思っています。（もう一つの理由は、私自身がアルゴリズムに関しては "初心者" であるため、難易度が高すぎると対処できません）

Leetcode を選んだ結果、私たちは Haskell ではなく Racket や Scala を使ってコードを提出しなければならないことがあります。これは時々問題を引き起こす可能性がありますが、私の経験では、私が提示するほとんどの Haskell コードは Scala に直接翻訳できます。各問題に対して、私は対応する Scala の解も提供します。もし問題が生じた場合、たとえば Scala に `Data.Graph` という不変データ構造のライブラリがない場合など、私たちは状況により非純粋な関数型プログラミングに戻ることもあります。しかし、そのような状況は全体的には少ないはずです。各問題の最後には、対応する命令型のバージョンも提供するかもしれません。その場合、私たちは関数型バージョンと命令型バージョンの違いについても議論するかもしれません。

これらの記事は "関数型プログラミング入門" ではありません。私はおそらくいくつかのライブラリ関数の使い方を示すかもしれませんが、全体的には、これらの記事はあなたがすでに純粋な関数型プログラミングの基本的な知識を持っていることを前提としています -- 再帰、リスト、fold、map など。あなたが関数型プログラミングや Haskell 言語について何も知らない場合、これらの記事を読む前に、いくつかの Haskell の知識を学ぶことをお勧めします。以下は、私が特に推奨する学習リソースです：

- [cis194 コース](https://www.seas.upenn.edu/~cis1940/spring13/lectures.html)
- *Thinking Functionally With Haskell*、およびその中国語翻訳版 [Haskell関数型プログラム設計](https://book.douban.com/subject/26769112/)
- 練習は重要です。[Haskell 99 問](https://wiki.haskell.org/99_questions/1_to_10)を通じて練習することができます。これらの練習は Leetcode とは異なり、ほとんどが直接の再帰で完了できます

> 道は遠く、修業は長し、上下に求めて進む

## 31. 次の順列

### 問題の説明

以下は、[Leetcode](https://leetcode.cn/problems/next-permutation/) の元の説明です：

> 整数配列の **順列** とは、そのすべてのメンバーを一連または線形の順序で並べることです。
>
> 例えば、`arr = [1,2,3]` の場合、以下のようなものはすべて `arr` の順列と見なすことができます：`[1,2,3]`、`[1,3,2]`、`[3,1,2]`、`[2,3,1]` 。
> 整数配列の 次の順列 とは、その整数の次の辞書順の大きい順列を指します。より形式的に言えば、配列のすべての順列が辞書順で小さい順に並べられたコンテナに存在すると仮定すると、その配列の 次の順列 とは、その順序付けられたコンテナの中でその配列の直後に存在する順列を指します。次の大きな順列が存在しない場合、その配列は辞書順で最小の順列（つまり、その要素が昇順に並んでいる）に再配置する必要があります。
>
> 例えば、`arr = [1,2,3]` の次の順列は `[1,3,2]` です。
> 同様に、`arr = [2,3,1]` の次の順列は `[3,1,2]` です。
> そして `arr = [3,2,1]` の次の順列は `[1,2,3]` です、なぜなら `[3,2,1]` は辞書順でより大きな順列が存在しないからです。
> 整数配列 `nums` を与えられた場合、`nums` の次の順列を見つけ出してください。
>
> 必ずインプレースで値を変更し、追加の定数スペースしか使用しないようにしてください。

私たちは純粋な関数型プログラミングを行っています。純粋な関数型プログラミングでは "変更" という概念は存在しないため、最後の文は無視します。

### 最初の考え

一般的に、多くのアルゴリズムの問題は、非常に簡潔（しかし効率が低い）純粋な関数型解を与えることができます。私たちはその解をプログラムの標準参照（Spec）と呼び、私たちの思考はそこから始まります。しかし、この問題はそのような状況ではないようです。この問題の数学的な表現を考えます：

$$
\text{next}(a) = \begin{cases}
  \min(\mathbb{P}) & a = \max(\mathbb{P}) \\
  \min(\{ m | m \in \mathbb{P}, m > a \}) & \text{それ以外}
\end{cases}
$$

これは私たちにあまりインスピレーションを与えません。実際、多くのプログラミング言語では、$\mathbb{P}$、つまりあるリストのすべての順列を求めるのが、この問題よりも面倒な場合があります。[^t]

そのような状況下、**単純な再帰** が問題を解決できるのでしょうか？

### 再帰

次の順列関数（`next` と記す）は、順列と辞書順に基づいて定義されていますが、それはより一般的な構造に拡張される可能性があります。

まず、10進数の整数の状況を検討します。明らかに、10進数の整数にも `next` 関数を定義できます。これは大名鼎鼎の "後続" 関数で、すなわち $f(x) = x + 1$ です。

10進数の整数は、リスト（または、命令型言語の配列）として表現できます。リストの各要素は、数字の各桁を示します。たとえば、$100$ は `[1, 0, 0]` として表現されます。

私たちは特に、リスト表現下の後続関数に注目します。なぜなら、そのような表現下では、`>=` はちょうど辞書順の比較であるからです。私たちはリスト表現下、10進数の整数の後続関数を `next10` と記します。

```Haskell
-- next10 [1, 1, 0] = [1, 1, 1]
next10 ...
```

たとえば、`next10 [1, 1, 0] = [1, 1, 1]` です。

この例は私たちに非常に強い直感を与えます。それは `next10 [1, 0] = [1, 1]` のとき、`next10 [1, 1, 0] = 1 : next10 [1, 0]` となることです。これを一般化すると、`x:xs` で表現された整数に対して、

```Haskell
next10 (x:xs) = x:(next10 xs)
```

上記の式はほとんどの場合に成り立ちます。ただし、`next10 xs` が "求められなくなる" とき、たとえば `[1, 9, 9]` の次の数（後続）は `[2, 0, 0]` で、そのときには "繰り上がり" が必要となります。より厳密に言えば、`99` は **最大** の2桁の10進数整数であるため、上記の等式は成り立ちません。`xs` がまだ最大値でない限り、上記の等式は成り立つことが証明できます。

同様に、問題で求められている `next` 関数も同様の性質を持つことが証明できます：`xs` が長さ `length xs` の順列の最大値でないとき、

```Haskell
next (x:xs) = x:next xs
```

この性質はあまりにも重要で、私たちは直接再帰関数を構築することができます：

```Haskell
next (x:xs)
    | isMax xs  = tick x xs
    | otherwise = x:next xs
```

それでは、問題は次の2つになります

1. `isMax` をどのように定義するか
2. `xs` がすでに最大値である場合、`tick` 関数をどのように定義するか

### `isMax` の定義

順列 `xs` に対して、「最大」は、どのように再配置しても、新しい `xs'` に対して `xs >= xs'` が成り立つことを意味します。

どのような順列がそのような性質を持つのでしょうか？直感的な考え方は、これは辞書順であるため、順列が可能な限り大きくなるためには、大きな数を前に置くべきだということです。言い換えれば、「降順」の順列は必ず最大であるはずです。

```Haskell
isMax :: Ord a => [a] -> Bool
isMax = down
```

Haskellでリストの昇順または降順を判断するには、「zip with tail」の方法を使用できます。具体的には、リストとその`tail`を`zip`し、その後`map`や`all`などの関数を使用して判断します。

```Haskell
dup :: [a] -> [(a, a)]
dup xs = zip xs (tail xs)

down :: Ord a => [a] -> Bool
down = all (uncurry (>=)) . dup
```

### `tick` の定義

もし `xs` が `down` を満たしている場合、言い換えれば、それが最大である場合、`x:xs` を次の列挙に変換するためには複雑な操作が必要となります。この複雑な操作を、私は`tick`と名付けました。

例えば、`[2, 4, 3, 1]`の次の列挙は`[3, 1, 2, 4]`です。このとき、`x`は`2`、`xs`は`[4, 3, 1]`で、`xs`が`down`を満たしているため、単純な再帰ではなく、`tick`を使用して計算する必要があります。

`[2, 4, 3, 1]`を引き続き例にとってみましょう。直感的に、`[2, 4, 3, 1]`の次の列挙`ys@(y:ys')`を得るためには、次の2つのステップに分けられます：

- `y`が何であるかを決定する
- `ys'`が何であるかを決定する

まず、`y`は`x`より厳密に大きでなければなりません。なぜなら、私たちはすでに、`x`で始まるすべての列挙が`x:xs`より小さいことを示したからです。次に、`y`は可能な限り小さくなければなりません、そうでなければ、`p`というより小さい列挙が存在し、`p > x:xs`となります。

上記の例では`y`は`3`です。すぐに思いつくのは、`3`は`xs = [4, 3, 1]`の中で、**最小の** `x = 2`より大きい数です。私たちはこのような数を`pivot`と呼びます。定義コードを以下に示します：

```Haskell
pivot x xs = minimum [ x' | x' <- xs, x' > x  ]
```

`xs`が順序付けられていることに注意してください。上記のコードは以下のように書き換えることができます。

```Haskell
pivot x = last . takeWhile (> x)
```

指摘しておくべきは、`pivot`関数は部分関数であり、不安全な`last`と`minimum`操作を使用しているため、呼び出す際には少なくとも1つの`x' > x`が存在することを保証しなければなりません。

`y > x`を見つけることができれば、`y:ys'`は必ず`x:xs`より大きくなることを保証できます。したがって、`ys'`の構築は`y:ys'`を可能な限り小さくするように行います。言い換えれば、`ys'`は最小の列挙でなければなりません。最大値についての上記の議論と同様に、最小の列挙は単調増加の列挙です。これを行うためにはソートが必要です。

`ys'`の要素は`xs`から`y`を取り除いた後、`x`を追加したものです。例えば、上記の例`[2, 4, 3, 1]`では、

```Haskell
x:xs = [2, 4, 3, 1]

y = last . takeWhile (> x) $ xs
  = last [4, 3]
  = 3

ys'' = delete 3 xs
     = [4, 1]

ys' = sort (x:ys'')
    = sort [2, 4, 1]
    = [1, 2, 4]

ys = y:ys'
   = [3, 1, 2, 4]
```

最後に、`pivot x xs = ⊥`という状況について議論する必要があります。この状況は事前に判断する必要があります、そうでなければエラーが発生します。`pivot`が存在しないのはいつでしょうか？それは`x:xs`が`down`を満たしているときです、例えば`[4, 3, 2, 1]`のとき、このとき、入力を反転するだけで済みます。

以上の議論に基づき、`tick`関数の定義を以下に示します：

```Haskell
import Data.List (delete)

tick :: Ord a -> a -> [a] -> [a]
tick x xs
    | null l    = reverse (x:xs)
    | otherwise = y:(sort (x:ys'))
    where l = takeWhile (> x) xs
          y = last l
          ys' = delete y xs
```

```Haskell
> tick 4 [3, 2, 1]
[1, 2, 3, 4]

> tick 2 [4, 3, 1]
[3, 1, 2, 4]
```

疑いの余地なく、現在の`next`関数はこの問題を正しく解決しています。読者は上記のHaskellコードをScalaに翻訳し、Leetcodeで試してみてください。再帰から出発し、この問題の解は非常に直感的で、簡単に且つ楽に提供できます。

### より効率的なコード

上記で定義した`next`関数は優雅で直感的ですが、十分に効率的ではありません。主な問題は、

1. `tick`では、`sort`を使用する必要はありません。`xs`は順序付けられているため、この点を利用して新しい順序付けられた列挙を$O(n)$内で組み合わせることができます
2. `down`は`next`によって何度も呼び出され、各呼び出しは$O(n)$のため、全体の複雑度が$O(n^2)$になります。

問題1.は非常に簡単で、`tick`を再定義するだけで済みます。

問題2.は少し複雑です。計算の過程を詳しく観察する必要があります、例えば`next [3, 5, 1, 4, 2]`：

```Haskell
next [3, 5, 1, 4, 2]
= 3:(next [5, 1, 4, 2])
= 3:(5:(next [1, 4, 2]))
= 3:(5:(tick 1 [4, 2]))
```

計算は常に一種の「形状」を持ち、それは入力の前半部分が不変で、後半部分が`tick`されることです。

```Haskell
[3, 5,           1, 4, 2]
^^^^^^           #######
不変の部分     tickの部分
```

直感的には、その境界線は`down`が`False`から`True`に変わる瞬間です。再び`next`関数に戻ります：

```Haskell
next (x:xs)
    | down xs  = tick x xs
    | otherwise = x:next xs
```

各再帰のとき、`next (x:xs)`は`down xs`を計算し、先ほどの例を再利用します、

```Haskell
-- 第一次再帰
down [5, 1, 4, 2] = False
-- 第二次再帰
down [1, 4, 2]    = False
-- 第三次再帰
down [4, 2]       = True
```

実際、`down`は常に`tails`の次の要素を順序に計算します：

```Haskell
> tails [3, 5, 1, 4, 2]
[[3,5,1,4,2],[5,1,4,2],[1,4,2],[4,2],[2],[]]
```

`next`が各回使用する`down xs`をあらかじめ[^1]計算し、リストに保存します。

```Haskell
import Data.List (tails)

nextDown (x:xs) (d:ds)
    | d = tick x xs
    | otherwise = x:nextDown xs ds

next xs = nextDown xs (downs xs)
downs = drop 1 . map down . tails
```

もし`downs`を$O(n)$時間内で計算できれば、問題2.は解決します。直感的には、これは非常に簡単です、なぜなら`down [4, 3, 2, 1]`を計算するとき、`down [3, 2, 1]`はすでに計算されており、それを「前回の計算」で保存し、後で使用する方法が必要だからです。

関数型プログラミングコミュニティでは、この種の問題に対する解決方法が既に存在します： scan theorem.

Scan theoremは以下を指します

```Haskell
map (foldl op a) . inits = scanl op a
```

同様に

```Haskell
map (foldr op a) . tails = scanr op a
```

この定理を利用して、以下にプログラムの計算過程を示します。注意、問題を避けるために、以下を定義しました

```Haskell
tails' = filter (not . null) . tails
```

```Haskell
downs = { definition }
        drop 1 . map down . tails'
      = { definition }
        drop 1 . map (all (uncurry (>=)) . dup) . tails'
      = { map distributivity }
        drop 1 . map (all (uncurry (>=))) . map dup . tails'
      = { need another proof (1) }
        drop 1 . map (all (uncurry (>=))) . tails . dup
      = { definition }
        drop 1 . map (foldr (&&) True . map (uncurry (>=))) . tails . dup
      = { fold-map fusion }
        drop 1 . map (foldr join True) . tails . dup
      = { scan theorem }
        drop 1 . scanr join True . dup
          where join (x, x1) r = x >= x1 && r
```

(1) 新たな証明が必要で、それは以下のコードが等価であることを示しています：

```Haskell
> map dup $ tails' [4, 2, 3, 1]
[[(4,2),(2,3),(3,1)],[(2,3),(3,1)],[(3,1)],[]]

> tails $ dup [4, 2, 3, 1]
[[(4,2),(2,3),(3,1)],[(2,3),(3,1)],[(3,1)],[]]
```

証明は省略します。

### 結論

上記の議論を組み合わせることで、効率的な関数型実装を以下に示します：

```Haskell
tick x xs = build l r x
    where (l, r) = span (> x) xs
          build [] r x = reverse (x:r)
          build l  r x = last l : reverse (init l ++ [x] ++ r)

dup xs = zip xs (tail xs)
downs = scanr (\(x, x1) r -> (x >= x1) && r) True . dup

next xs = l ++ tick (head r) (tail r)
    where (l, r) = splitAt (n - 1) xs
          n = length $ takeWhile not $ downs xs
```

これは$O(n)$時間の複雑度です。しかし、私たちの定数は命令型プログラムよりも確かに大きいかもしれません。理論的には、前節で定義した`nextDown`関数の実装の方が効率的かもしれません（`length`と`splitAt`を避ける）、しかし、私は本節で示した実装の方がより明確だと思います。

私は等価なScalaコードをleetcodeに提出しました：

```Scala
object Solution {
    def nextPermutation(nums: Array[Int]): Unit = {
        val nextNums = next(nums.toList).toArray
        for (i <- nums.indices) {
            nums(i) = nextNums(i)
        }
    }

    def dup(xs: List[Int]): List[(Int, Int)] = xs zip xs.drop(1)

    def tick(x: Int, xs: List[Int]): List[Int] = {
        val (l, r) = xs.span(_ > x)
        if (l.isEmpty) r.reverse ++ List(x)
        else l.last :: (l.init ++ List(x) ++ r).reverse
    }

    def tailsDown(xs: List[(Int, Int)]): List[Boolean] = 
        xs.scanRight(true) { case ((x, x1), r) => (x >= x1) && r }

    def next(xs: List[Int]): List[Int] = {
        val n = tailsDown(dup(xs)).takeWhile(!_).length
        val (l, r) = xs.splitAt(n - 1)
        l ++ tick(r.head, r.tail)
    }
}
```

「次の順列」問題は、命令型プログラムが関数型プログラムよりも簡潔になる数少ない問題の一つです。実際、Haskellの[Data.Permute](https://hackage.haskell.org/package/permutation-0.5.0.5/docs/Data-Permute.html)ライブラリも、命令型の方法を使用しています。

命令型プログラムでは、上記のアルゴリズムは以下の2つのプロセスとして説明できます：

1. 右から左に走査し、最も長い降順列`a[i ... (len - 1)]`を見つける
2. 計算を行う、
   - `i = 0`の場合、配列を反転する
   - それ以外の場合、`j ∈ [i, (len - 1)]`を見つける、そのような
     - $a[j] > a[i - 1]$
     - $\forall k \ge i, a[k] > a[i - 1] \to a[k] \ge a[j]$
     $i - 1, j$ を交換し、$a[i \dots (\text{len - 1})]$ を反転する

上記の関数型アルゴリズムを完全に理解している読者にとっては、この命令型アルゴリズムは非常に直感的のはずです。しかし、このアルゴリズムをそのままあなたに提示し、理解できるでしょうか？

それでは、Pythonのプログラムで今日の話を締めくくりましょう！

```Python
def swap(a, i, j):
    tmp = a[i]
    a[i] = a[j]
    a[j] = tmp

def rev(a, start, end):
    l = end - start
    for i in range(start, start + l // 2):
        swap(a, i, end - (i - start) - 1)

def solve(a):
    for i in range(len(a) - 1, -1, -1):
        if i >= 1 and a[i - 1] < a[i]:
            break
    if i == 0:
        rev(a, 0, len(a))
    else:
        j = len(a) - 1
        while j >= i and a[j] <= a[i - 1]:
            j -= 1
        swap(a, i - 1, j)
        rev(a, i, len(a))
```

[^1]: 注意:これは正確な説明ではない、Haskellは遅延評価であり、評価プロセスは慎重に分析する必要がある
[^t]: 私は、関係的プログラム計算がこの問題に適用可能かどうかは不明である
---
title: Hindley-Milner型システムを10分で理解する
author: ayanamists
tags: 
  - 型推論
  - プログラミング言語
date: 2021-11-25
categories:
  - 学術
math: true
---

<div class="admonition" data-admonition-type="warning">
この記事はChatGPTによって中国語から翻訳されたもので、いくつかの誤りが含まれているかもしれません。不正確な部分があればご了承ください。
</div>
## 型推論

### 型推論とは何か

プログラマーは怠惰な生物です。多くの場合、私たちは冗長に型を指定したくありません：

```fsharp
let t = 1 + 1
let f x = x + 1
```

ここでの `t`、`f`、`x` の3つの変数は、それぞれの型が非常に明確です。もちろん、より多くの場合、型を明示的に指定する方が良いです。

また、型を指定するのが難しい場合もあります。例えば：

```fsharp
let s f g x = f x (g x)
let k x y = x
let i = s k k
let ϕ = s (k (s i)) k
```

ϕの型は何でしょうか？

ϕの型は計算できますが、非常に面倒です。自然な考え方は、人間の脳を代替するアルゴリズムを見つけることです。

実際、このコードをhaskellやf#のreplに送ると、ϕの型が`p -> (p -> q) -> q`であることを教えてくれます。そして、彼らが使用しているアルゴリズムは、Hindley-Milner型推論アルゴリズムです。

### 型推論アルゴリズム

一言で言えば、型推論アルゴリズムは方程式を解くことです。方程式の構築過程は次のようになります：

1. 各関数のパラメータ、および各関数呼び出しの戻り値を型変数`xᵢ`で表します。
2. 関数呼び出し時に方程式を構築します。`f x`という関数呼び出しは、以下のルールで方程式を構築できます：

$$
\frac{f:a \rightarrow b\quad x:t_1 \quad (f\ x):t_2}{t_1 \rightarrow t_2 = a \rightarrow b}
$$

例えば、このような関数：

```fsharp
let f a b = a + b
```

は、このような方程式を構築できます：

$$
\begin{aligned}
a:\\ &t_1 \\\\
b:\\ &t_2 \\\\
f:\\ &t_3 \\\\
a + b:\\ &t_4 \\\\
(+):\\ & \mathsf{int} \rightarrow \mathsf{int} \rightarrow \mathsf{int} \\\\
t_1 \rightarrow t_2 \rightarrow t₄ =&\ \mathsf{int} \rightarrow \mathsf{int} \rightarrow \mathsf{int} \\\\
t_3 =&\\ t_1 \rightarrow t_2 \rightarrow t_4
\end{aligned}
$$

以下の二つの方程式を解くことで、各変数の型を得ることができます。現在の問題は、このような方程式をどのように解くかです。

### 単一化

単一化は自動定理証明からのアルゴリズムで、上記の方程式を解くために使用できます。背後には一連の置換に関する理論があります。スペースの制約上、ここではこれらの理論については議論せず、最も自然なアルゴリズムを短く紹介します[^1]。

まず、上記の方程式では、最大で2種類の項が参加します。一つは型変数`xᵢ`、もう一つは矢印`xᵢ → xⱼ`です。実際には、矢印`xᵢ→ xⱼ`は`f(xᵢ, xⱼ)`と見なすことができ、これにより2つの形式が得られます：

1. 型変数。
2. `f(v₀, v₁, v₂, ...)`の形式。上記の`int`は`int()`と見なすことができます。

アルゴリズムは次の通りです：

各方程式を走査します：

1. 自己ループチェックを行い、両側に
    $$
    x_i = \cdots x_i \cdots
    $$
  （例えば $x_i = f(x_i)$ ）の形式が出現した場合、アルゴリズムは失敗します。

2. 方程式の形式が:
$$
\begin{aligned}
  x_i &= \star \\\\
  \star &= x_i
\end{aligned}
$$  
   である場合、$\star$が何であれ、*まだ走査していない方程式*の中の$x_i$を$\star$で置き換えます。

3. それがケース1.でない場合、つまり
   $$
   f(x_0, x_1, x_2, \cdots) = g(y_0, y_1, y_2, \cdots)
   $$
   であれば、
   * $f ≠ g$の場合、アルゴリズムは失敗します。
   * $f = g$の場合、$n$（ $n$ は f のパラメータの数 ）個の新しい待遍方程式 $x_i = y_i$を追加します。

[^1]: 文末で紹介する実装は、Union-Findアルゴリズムに基づいています。

## Let多相性

これで型推論アルゴリズムができました。次のような関数を考えてみましょう：

```fsharp
let f x = x
```

上記の推論アルゴリズムでは、`f:x₀ → x₀`と推論され、ここには自由変数 `x₀`があります。System Fの型表記法を用いて、このような関数を「一般化」します：

$$
\frac{f:\psi(x_0)}{f:\forall x_0.\psi(x₀)}
$$

これにより、`f`の型は「多相型」になり、「具体化」することができます：

$$
\frac{f:\forall x.\psi(x)}{f:\psi(a)}
$$

面倒な問題は、いつ一般化と具体化を行うべきかということです。

自然な考え方は、`fun x -> ...`のようなラムダ式の外部でジェネリック化を行い、ジェネリック化された変数を参照するときにインスタンス化を行うことです。例えば、

```fsharp
// 以下はすべて型が正しい項目です
(λ x. x) 10
(λ x. x) true
```

このようにすると、問題は、ラムダ式だけが"多型型"になり、ラムダ内部の変数はまだ単一型しか持てないということです。

```fsharp
(λf.(λ t. f true)(f 0))(λx.x)
```

上記の式を例に取ると、`λx.x`が多型型であっても、`f`は多型型として推論されません。実際には、`f`は`f 0`で`int → x₀`と推論され、`f true`で`bool → x₀`と推論されます。これにより型エラーが発生します。

F#やocamlでは、`f`にジェネリックを与える真の方法を思い出してみてください：

```fsharp
let f x = x in
    let t = f 0 in f true
```

無型または単純型のラムダ式では、上記の2つのコードは完全に等価で、`let`はただのシンタックスシュガーです。しかし、ML系の言語では、`let`はシンタックスシュガーではありません。実際、ML系の言語では、"ジェネリック化"を行うタイミングはまさに`let`のバインディングの時点です！

つまり：

1. 式`let var = e₁ in e₂`では、`e₁`の型がジェネリック化されます。ジェネリック化の方法は次のとおりです：
    * 現在の環境$Γ$の自由変数は$F(Γ)$
    * $e_1$の自由変数は$F(e_1)$
    * $F(e_1) - F(Γ)$の各自由変数$x_i$を走査し、$e_1:t$を$e_1:∀x_i.t$に変更します
2. 現在の環境$Γ$からジェネリック変数$e:t_1$を参照するたびに、新しい型変数で$e:t_1$をインスタンス化し、$t_1$が$∀$を含まなくなるまで続けます

これがHindley-Milnerアルゴリズムの基本的な考え方です。具体的な実装については、algorithm Jとalgorithm Wがあります。詳細はWikipediaやMilnerの[原著論文](https://www.sciencedirect.com/science/article/pii/0022000078900144?via%3Dihub)を参照してください。[私の実装](https://github.com/ayanamists/TypeInf)はalgorithm Jを採用しています。

## 値制約

もし本当に前のコードを試したなら：

```fsharp
let s f g x = f x (g x)
let k x y = x
let i = s k k
let ϕ = s (k (s i)) k
```

あなたは、Haskellでは問題ないことに気付くでしょう：

![1](https://pic.imgdb.cn/item/619f4b162ab3f51d911a8f2f.jpg)

しかし、F#では、これはエラーになります：

![2](https://pic.imgdb.cn/item/619f4b552ab3f51d911ab740.jpg)

なぜエラーになるのでしょうか？これは、もっとトリッキーなプログラムから説明します。F#には可変型があります：

```fsharp
let mutable x = 10
x <- 20
printfn "%A" x
```

このようなプログラムが与えられた場合：

```fsharp
let mutable x = []
x <- [3]
List.map (fun x -> x = true) x
```

最初の行はエラーになります。しかし、最初の行がエラーにならないと仮定すると、2行目はエラーになるでしょうか？これを詳しく調べてみましょう。

まず、`<-`の型はF#では隠されていますが、F#には`!`、`:=`、`ref`という[廃止された](https://aka.ms/fsharp-refcell-ops)演算子があり、その型は次のようになります：

```fsharp
ref  : 'a -> 'a ref
(!)  : 'a ref -> 'a
(:=) : 'a ref -> 'a -> unit
```

上記のプログラムは次のように書くことができます：

```fsharp
let x = ref []
x := [3]
List.map (fun x -> x = true) x
```

2行目で参照されるとき、`x`の型は`'t list ref`、つまり`∀t.ref[list[t]]`であり、これは`ref[list[b]]`に例化されるべきです。これにより、次の2つの等式が得られます：

$$
\begin{aligned}
\mathsf{ref}[\mathsf{list}[b]] =& \mathsf{ref}[a] \\\\
a =& \mathsf{list}[\mathsf{int}]
\end{aligned}
$$

上記の方程式から、$b = \mathsf{int}, a = \mathsf{list}[\mathsf{int}]$ が得られ、**これはエラーになりません**。さらに大きな問題は、2行目の型チェックの後、`b`は確かに`int`になりますが、これは **例化された** `x`であり、$Γ$ 中の`x`の型は常に`'t list ref`であるため、**3行目もエラーにならない！**

これは非常に悪いことで、型システムの不健全性を導入します。3行目は間違いなくランタイムエラーを引き起こします。

この問題を解決するために、ANDREW K. WRIGHT[^2]は値制限（value restriction）を導入しました。値制限の意味は非常にシンプルです：`let var = e₁ in e₂`の`e₁`が構文値（syntactic value）である場合にのみ、`e₁`を一般化します。もし`e₁`が構文値でなく、かつそれが$\Gamma$にない自由変数を持っているなら、エラーを報告します。

構文値とは、ML系の言語では、定数、変数、λ式、型コンストラクタを指します。もちろん、`ref[a]`型の値は構文値とは見なされません。これにより、問題は明らかに解決されます。なぜなら、上記のエラーは本質的に副作用の発生によるものであり、MLでは構文値の評価に副作用は発生しないからです。

この解決策は過近似であり、多くの正しいプログラム（例えば前述のプログラム）も拒否されます。Haskellは副作用がないため、この問題は存在しません。これがHaskellが`f . g`のような関数の組み合わせを大量に使用できる一方、MLではできない主な理由です。

[^2]: Simple imperative polymorphism, ANDREW K. WRIGHT

## 量詞の位置は？

System Fでは、量詞は型の任意の位置に出現できます。例えば、$∀x.(x → ∀y.y)$。実際、System FはYコンビネータを構築することはできません（強正規性に反する）が、無型ラムダ計算の$(x x)$構造をエンコードすることはできます：

$$
λ(x:∀α.α→α).(x (∀α.α→α))x
$$

このλ式の型は $(∀α.α→α)→(∀α.α→α)$で、その最初の引数の型は、それが`Λα.λ(x:α).x`という`id`関数にのみ作用できることを決定します。それを`id`に適用する形式を無型λ式で書くと：

$$
(λx.xx)(λx.x)
$$

間違いなく、それはβ簡約によって$λx.x$になります。

これにより、System Fのλ式では、その引数の型が「多型型」を含むことができることがわかります。では、なぜ私たちの前の型推論アルゴリズムがその多型型を推論できなかったのでしょうか？

これは、私たちの前の推論アルゴリズムが「単純型λ式」の型推論アルゴリズムと見なすことができ、HM型システムは単純型λ式の拡張であるからです。

ここまで来ると、自然に疑問が生じます：System Fは同様の推論を行うことができるのでしょうか？つまり、私たちはこのように書く：

$$
λ(x:t_1).(x\ t_2)x
$$

この式を通じて $t_1$ と $t_2$ の値を得ることができるアルゴリズムが存在するのでしょうか？

1985年、Hans-J. Boehmの論文 *PARTIAL POLYMORPHIC TYPE INFERENCE IS UNDECIDABLE* は、`fix`コンビネータを追加すると、上記の問題は判定不能になると教えてくれます。1995年、J.B.Wellsの論文 *Typability and type checking in System F are equivalent and undecidable* は、明示的な $t₁$ と $t₂$ を与えずに、最終的な型のチェックだけを求めると、この問題はまだ判定不能であるとさらに教えてくれます。

したがって、HM型システムは2種類のλ式の変種と見なすことができます：

* 単純型λ式の変種と見なす場合、HM型システムは`let var = e₁ in e₂`という形式を追加し、`e₁`の一般化を許可し、パラメータ多態性を実現します。
* System Fの変種と見なす場合、HM型システムは量子の位置を制限します--型の最前面だけです。この制限により、HM型システムは推論を実現でき、System Fは型推論を実現できません。

しかし、これは、より強力な型システムが型推論を行うことができないという意味ではありません。実際、Haskellはより強力な型推論を実装し、Scala、Typescript、Rustなどの言語は "局部型推論"（local type inference）と呼ばれる技術を使用しており、これはプログラマが一部の型マークを提供することで、不必要な型マークの大部分を消去することができます。

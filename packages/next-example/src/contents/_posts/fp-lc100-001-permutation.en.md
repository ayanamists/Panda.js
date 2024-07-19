---
title: "Functional Leetcode 100 -- Permutations"
date: 2024-04-24T19:38:38+08:00
draft: false
categories:
    - Algorithm
tags:
    - Haskell
    - Functional Algorithm
---

<div class="admonition" data-admonition-type="note">
This article has some technical details, but the important thing is the story. If you don't understand, you can skip it, it won't affect the understanding of the story. However, I suggest at least understanding the content of the first section, so you know what Church is roughly doing.
</div>



## Preface

Algorithm problems are one of the nightmares for many programmers. Algorithms themselves are certainly interesting, but the frustration of not being able to solve problems and the embarrassment of not being able to answer questions in interviews is something no one enjoys.

The traditional way to deal with the challenge of algorithm problems is **problem-solving practice**. Solving a large number of problems to learn algorithms. Many people believe in a theory that, whether it's learning mathematics or algorithms, a large amount of practice is necessary.

I also agree with this theory. The famous saying of von Neumann is still hanging on the homepage of this blog:

> Young man, in mathematics you don't understand things. You just get used to them.

Intuitive understanding, intuitive experience, is an invaluable treasure. Practice is the most reliable means of acquiring this invaluable treasure. In short, if you want to learn algorithms, you must practice problems.

However, I am extremely tired of a conventional path of algorithm learning. That is, when you encounter a problem you can't solve, after thinking about it for 10 minutes and still not knowing where to start, you go looking for the answer. Perhaps you "understand" the answer or remember the answer, and the next time you see the same problem, you plug it in. As long as you have seen enough answers, then you can "plug" enough problems.

Why don't I like this learning method? Perhaps I'm not smart enough, I find it difficult to "understand" the answers given by others. Too many people only tell you the final efficient algorithm, ignoring the process of building intuition, let alone proving the algorithm. From these people's answers, I neither feel their deep understanding of the problem nor the beauty of the algorithm itself.

Can we only learn algorithms through an education designed for "geniuses"?

No. There are still people in the world who are trying to design programs using purely functional languages and equation derivation methods. These people also value the value of intuition. They start from a clear but inefficient program and step by step give the final efficient algorithm. In this process, the beauty of the algorithm, the insight of the algorithm, is revealed in detail. The late Richard Bird and Professor Mu Xincheng from Taiwan are pioneers of this approach. In their papers and books, I feel a true gentleness.

I think, although equation derivation is often very difficult, explaining what the non-trivial part of the algorithm is and what the intuition of the algorithm is, may not be that difficult.

This series of articles is my attempt. In these articles, I will explain how I solve Leetcode problems using purely functional programming. As for why I chose Leetcode instead of the more difficult Codeforces, I believe that too high a difficulty may not be a good thing. High difficulty problems are likely to require multiple algorithms to handle together, and we want to focus on a specific type of algorithm in each article. (Another reason is that I am still a "beginner" in the sense of algorithms, and I can't handle problems that are too difficult)

The consequence of choosing Leetcode is that we must use Racket or Scala rather than Haskell to submit code. This can sometimes cause some confusion, but my experience is that most of the Haskell code I provide can be directly translated into Scala. For each problem, I will also provide the corresponding Scala solution. If there are indeed problems, such as Scala not having a `Data.Graph` immutable data structure library, then we will fall back to non-pure functional programming as the situation dictates. However, this situation should be relatively rare overall. At the end of each problem, I may also provide the corresponding imperative version, and perhaps we will also discuss the differences between the functional version and the imperative version.

These articles are not "Introduction to Functional Programming". Although I may hint at the usage of certain library functions, overall, these articles assume that you already have a basic knowledge of purely functional programming - recursion, lists, fold, map, etc. If you know nothing about functional programming and the Haskell language, it is best to learn some Haskell knowledge before reading these articles. Here are some learning resources I recommend:

- [cis194 course](https://www.seas.upenn.edu/~cis1940/spring13/lectures.html)
- *Thinking Functionally With Haskell*, as well as its Chinese translation version [Haskell Functional Program Design](https://book.douban.com/subject/26769112/)
- Practice is important, you can practice through [Haskell 99 Questions](https://wiki.haskell.org/99_questions/1_to_10). These exercises are different from Leetcode, most of them can be completed through direct recursion.

> The road is long and the way is far, I will seek it up and down.

## 31. Next Permutation

### Problem Description

Here is the original description from [Leetcode](https://leetcode.com/problems/next-permutation/):

> A permutation of an array of integers is an arrangement of its members into a sequence or linear order.
> 
> For example, for `arr = [1,2,3]`, the following are all the permutations of `arr`: `[1,2,3]`, `[1,3,2]`, `[2,1,3]`, `[2,3,1]`, `[3,1,2]`, `[3,2,1]`.
> 
> The next permutation of an array of integers is the next lexicographically greater permutation of its integer. More formally, if all the permutations of the array are sorted in one container according to their lexicographical order, then the next permutation of that array is the permutation that follows it in the sorted container. If such arrangement is not possible, the array must be rearranged as the lowest possible order (i.e., sorted in ascending order).
> 
> For example:
> - The next permutation of `arr = [1,2,3]` is `[1,3,2]`.
> - Similarly, the next permutation of `arr = [2,3,1]` is `[3,1,2]`.
> - While the next permutation of `arr = [3,2,1]` is `[1,2,3]` because `[3,2,1]` does not have a lexicographical larger rearrangement.
> 
> **Given** an array of integers `nums`, find the next permutation of `nums`.
> 
> **The replacement must be in place and use only constant extra memory.**


We are in purely functional programming. Pure functional programming has no concept of "modification", so we can ignore the last sentence.

### First Thought

Generally, many algorithm problems can give an extremely concise (but inefficient) purely functional solution. We call this solution the program's standard reference (Spec), and our thinking starts from here. However, this problem does not seem to be the case. Consider the mathematical expression of this problem:
$$
\text{next}(a) = \begin{cases}
  \min(\mathbb{P}) & a = \max(\mathbb{P}) \\
  \min(\{ m | m \in \mathbb{P}, m > a \}) & \text{otherwise}
\end{cases}
$$

This does not give us much inspiration. In fact, in many programming languages, finding all permutations of a list ($\mathbb{P}$), is more troublesome than this problem. [^t]

In this case, an intuitive attempt is, can **simple recursion** solve the problem?

### Recursion

The next permutation function (denoted as `next`) is defined on permutations and lexicographical order, but it can obviously be generalized to more generalized structures.

We first examine the case of decimal integers. Clearly, decimal integers can also define a `next` function. This is the famous "successor" function, i.e., $f(x) = x + 1$.

Decimal integers can be represented as a list (or, in imperative languages, an array), where each element in the list represents a digit of the number. For example, $100$ is represented as `[1, 0, 0]`.

We are particularly interested in the successor function under list representation, because under this representation, `>=` is precisely the lexicographical order comparison. We denote the successor function of decimal integers under list representation as `next10`.

```Haskell
-- next10 [1, 1, 0] = [1, 1, 1]
next10 ...
```

For example, `next10 [1, 1, 0] = [1, 1, 1]`.

This example gives us a very strong intuition. That is, `next10 [1, 0] = [1, 1]`, at this time `next10 [1, 1, 0] = 1 : next10 [1, 0]`. Generalizing this relationship, we get that for the integer represented by `x:xs`,

```Haskell
next10 (x:xs) = x:(next10 xs)
```

The above equation holds most of the time. Unless, `next10 xs` has already "failed to be calculated", for example, the next number (successor) of `[1, 9, 9]` is `[2, 0, 0]`, at this time it is necessary to "carry over". More strictly speaking, `99` is the **maximum** two-digit decimal integer, so the above equation does not hold. It can be proven that as long as `xs` is not the maximum value, then the above equation holds.

Similarly, it can also be proven that the `next` function required by the problem has the same property: when `xs` is not the maximum value of a permutation of length `length xs`,

```Haskell
next (x:xs) = x:next xs
```

This property is far too important, because we can directly construct the recursive function:

```Haskell
next (x:xs)
    | isMax xs  = tick x xs
    | otherwise = x:next xs
```

So, the problem becomes

1. How to define `isMax`
2. If `xs` is already the maximum value, how should the `tick` function be defined?

### Definition of `isMax`

For a permutation `xs`, "maximum" means that no matter how it is rearranged, the new `xs'` must have `xs >= xs'`.

What kind of permutation has this property? A very natural idea is that because this is lexicographical order, if you want the permutation to be as large as possible, you must put the larger numbers in front. In other words, "descending" permutations must be the largest.

```Haskell
isMax :: Ord a => [a] -> Bool
isMax = down
```

In Haskell, to determine whether a list is ascending or descending, we can use the "zip with tail" method. Specifically, we first zip the list with its `tail`, and then use functions like `map`, `all` to make the judgment.

```Haskell
dup :: [a] -> [(a, a)]
dup xs = zip xs (tail xs)

down :: Ord a => [a] -> Bool
down = all (uncurry (>=)) . dup
```

### Definition of `tick`

If `xs` already satisfies `down`, in other words, it is already the largest, then `x:xs` must undergo a complex operation to get the next enumeration. This complex operation, which I call `tick`.

For example, the next enumeration of `[2, 4, 3, 1]` is `[3, 1, 2, 4]`. Here `x` is `2`, `xs` is `[4, 3, 1]`, and `xs` satisfies `down`, so it is no longer possible to perform a simple recursion, but rather to use `tick` for the calculation.

Continuing with the example of `[2, 4, 3, 1]`. Intuitively, to find the next enumeration of `[2, 4, 3, 1]` `ys@(y:ys')`, it can be divided into two steps:

- Determine what `y` is
- Determine what `ys'` is

First, `y` must be strictly greater than `x`. Because we have already shown that all enumerations starting with `x` are smaller than `x:xs`. Second, `y` should be as small as possible, otherwise there would be a smaller enumeration `p` that satisfies `p > x:xs`.

In the above example, `y` is `3`. It is easy to think that `3` is the **smallest** number in `xs = [4, 3, 1]` that is greater than `x = 2`. We call this number the `pivot`. We can write the definition code:

```Haskell
pivot x xs = minimum [ x' | x' <- xs, x' > x  ]
```

Notice that `xs` is ordered, so the above code can be rewritten as

```Haskell
pivot x = last . takeWhile (> x)
```

It should be noted that the `pivot` function is a partial function, we used unsafe `last` and `minimum` operations, which means that when calling it, we must ensure that there is at least one `x' > x`.

If we can find such a `y > x`, then we can be sure that `y:ys'` is definitely larger than `x:xs`. Therefore, the construction of `ys'` should make `y:ys'` as small as possible. In other words, `ys'` should be the smallest permutation. Similar to our discussion above about the maximum value, the smallest permutation is the monotonically increasing permutation. This only requires sorting.

The elements in `ys'` are naturally `xs` minus `y` and then plus `x`. For example, in the above example `[2, 4, 3, 1]`, it can give

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

Finally, there is one more situation that needs to be discussed, which is when `pivot x xs = ⊥`. This situation must be judged in advance to avoid errors. When does `pivot` not exist? That is when `x:xs` already satisfies `down`, for example `[4, 3, 2, 1]`, at this time, it is only necessary to reverse the input.

Based on the above discussion, we give the definition of the `tick` function:

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

Without a doubt, the current `next` function has already been able to correctly solve this problem. Readers may translate the above Haskell code into Scala and try it on Leetcode. Starting from recursion, we find that the solution to this problem is very intuitive, and can be given simply and easily.

### More Efficient Code

Although the `next` function defined above is elegant and intuitive, it is not efficient enough. The main problem lies in,

1. In `tick`, there is no need to use `sort`. `xs` is ordered, and this can be used to combine the new ordered sequence within $O(n)$
2. `down` is called many times by `next`, each call is $O(n)$, leading to an overall complexity of $O(n^2)$.

Problem 1. is simple, just rewrite `tick`.

Problem 2. is more complicated. We need to carefully observe the calculation process, for example `next [3, 5, 1, 4, 2]`:

```Haskell
next [3, 5, 1, 4, 2]
= 3:(next [5, 1, 4, 2])
= 3:(5:(next [1, 4, 2]))
= 3:(5:(tick 1 [4, 2]))
```

It can be seen that this calculation always has a certain "shape", that is, the front part of the input remains unchanged, and the back part is `tick`.

```Haskell
[3, 5,           1, 4, 2]
^^^^^^           #######
Unchanged part     tick part
```

And their boundary, intuitively, is the moment when `down` changes from `False` to `True`. Let's go back to the `next` function:

```Haskell
next (x:xs)
    | down xs  = tick x xs
    | otherwise = x:next xs
```

In each recursion, `next (x:xs)` will calculate `down xs`, using the example just now,

```Haskell
-- First recursion
down [5, 1, 4, 2] = False
-- Second recursion
down [1, 4, 2]    = False
-- Third recursion
down [4, 2]       = True
```

In fact, `down` always sequentially calculates the next element in `tails`:

```Haskell
> tails [3, 5, 1, 4, 2]
[[3,5,1,4,2],[5,1,4,2],[1,4,2],[4,2],[2],[]]
```

Let's calculate the `down xs` used by `next` in advance[^1] and put it in a list

```Haskell
import Data.List (tails)

nextDown (x:xs) (d:ds)
    | d = tick x xs
    | otherwise = x:nextDown xs ds

next xs = nextDown xs (downs xs)
downs = drop 1 . map down . tails
```

If we can calculate `downs` in $O(n)$ time, problem 2. will be solved. Intuitively, this is easy, because `down [4, 3, 2, 1]` is supposed to ensure:

- `4 >= 3`
- `down [3, 2, 1]`

In other words, when we want to calculate `down [4, 3, 2, 1]`, `down [3, 2, 1]` has already been calculated, it just needs a way to store this "previous calculation" for later use.

The functional programming community has long had a solution to this kind of problem: the scan theorem.

The scan theorem refers to

```Haskell
map (foldl op a) . inits = scanl op a
```

Similarly

```Haskell
map (foldr op a) . tails = scanr op a
```

Using this theorem, we give the following program calculation process. Note that, to avoid problems, we defined

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
      = { (1) }
        drop 1 . map (all (uncurry (>=))) . tails . dup
      = { definition }
        drop 1 . map (foldr (&&) True . map (uncurry (>=))) . tails . dup
      = { fold-map fusion }
        drop 1 . map (foldr join True) . tails . dup
      = { scan theorem }
        drop 1 . scanr join True . dup
          where join (x, x1) r = x >= x1 && r
```

A new proof for `(1)` is needed, which states that the following code is equivalent:

```Haskell
> map dup $ tails' [4, 2, 3, 1]
[[(4,2),(2,3),(3,1)],[(2,3),(3,1)],[(3,1)],[]]

> tails $ dup [4, 2, 3, 1]
[[(4,2),(2,3),(3,1)],[(2,3),(3,1)],[(3,1)],[]]
```

Proof omitted.

### Summary

Combining the above discussion, we can give an efficient functional implementation:

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

This is $O(n)$ time complexity. However, our constant is indeed larger than that of an imperative program. Theoretically, the implementation of the `nextDown` function defined in the previous section is more efficient (avoiding `length` and `splitAt`), but I still think that the implementation given in this section is clearer.

I submitted the equivalent Scala code to leetcode:

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

The "next permutation" problem is one of the few problems where an imperative program can be simpler than a functional program. Even the Haskell [Data.Permute](https://hackage.haskell.org/package/permutation-0.5.0.5/docs/Data-Permute.html) library uses an imperative method.

In imperative programs, the above algorithm can be described as two processes:

1. Traverse from right to left to find the longest descending sequence $a[i \dots (\text{len - 1})]$
2. Perform the calculation,
   - If $i = 0$, then reverse the array
   - In other cases, find $j \in [i, \text{(len - 1)}]$, such that

     - $a[j] > a[i - 1]$
     - $\forall k \ge i, a[k] > a[i - 1] \to a[k] \ge a[j]$

     Swap $i - 1, j$, reverse $a[i \dots (\text{len - 1})]$

If the reader fully understands the above functional algorithm, this imperative algorithm should be very straightforward. But if this imperative algorithm is directly presented to you, can you really understand why it is correct?

Let's end today's story with a Python program!

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

[^1]: Note that this is not a precise description, Haskell is lazy, and the evaluation process requires careful analysis
[^t]: I am not sure if Relational program calculus can be applied to this problem

#program-test #text-case-reduction

>[!def] Monotonicity
> A program $P$ is said to be _monotonic_ iff
> $$ \mathrm{test} (c_0) = \mathrm{Fail} \implies \forall c \supseteq c_0, \mathrm{test}(c) = \mathrm{Fail}  $$

>[!def] global minimum of a test case
> Given a test case $c_F$, a test case $c\subseteq c_F$ is called a _global minimum of_ $c_F$ iff
> $$ \forall c' \subseteq c_F, |c'| < |c| \implies \mathrm{test}(c') = \mathrm{Pass}  $$


>[!def] local minimum of a test case
> Given a test case $c_F$, a test case $c\subseteq c_F$ is called a _local minimum of_ $c_F$ iff
> $$ \forall c' \subseteq c, |c'| < |c| \implies \mathrm{test}(c') = \mathrm{Pass}  $$


# Delta Debugging

1. (_Reduce to subset_): Split the current configuration into $n$ partitions. Test each partition for failure. If a partition does induce the failure, then treat it as the current configuration and resume at Step 1. 
2. (_Reduce to complement_): Test the complement of each partition. If any induces the failure then treat it as the current configuration and resume at Step 1. 
3. (_Increase granularity_): Try splitting the current configuration into smaller partitions, $2n$ if possible, where $n$ is the current number of partitions. Resume at Step 1 with the smaller partitions. If the configuration cannot be broken down into smaller partitions, the current configuration is _1-minimal_ and the algorithm terminates.


```haskell
import Data.List as L
import Data.Maybe (listToMaybe)

type TestCase a = [a] -- type `a` is delta

-- | delta debug test-case reduction
dd :: (TestCase a -> Bool) -> TestCase a -> TestCase a
dd test tc = go 2 tc
 where 
  go :: Int -> TextCase a -> TestCase a
  go n tc = 
    let pairs       = splitN n tc
        segments    = filter test (fst <$> pairs)
        complements = filter test (snd <$> pairs)
    in  case (listToMaybe segments, listToMaybe complements) of 
      (Just tc', _) -> 
        if length tc' == 1 
          then tc'
          else go 2 tc'
      (_, Just tc') -> 
        go (n-1) tc'
      (Nothing, Nothing) ->
        let n' = 2 * n
        in if length tc < n'
             -- tc/n < 2 => tc/n = 1 
             -- thus we have run test for each elem of tc
             then tc 
             else go n' tc
  splitN :: Int -> TestCase a -> [(TestCase a, TestCase a)]
  splitN n tc = 
    let len = length tc / n
    in  [ (mid, left ++ right)
        | k <- [1..n]
        , let (left, xs) = L.splitAt ((k-1) * len) tc   
              (mid, right) = L.splitAt len xs
        ]
```

>[!thm] correctness of DD
> If the test function $P$ is _monotonic_ and $P(c)$ _fails_, then _delta debugging_ will terminate and return a _local minimum of test case_ $c$ 


# HDD (Hierarchical Delta Debugging)

> **Hierarchical Delta Debugging** is a refinement of Delta Debugging that takes both a test case to reduce and _a description of the test cases’s input format (as a context-free grammar)_. By exploiting the grammar it can remove and simplify large, structured chunks of data, without wasting time making deletions that are guaranteed to make the test input invalid

In DD the test case has a linear structure, while in HDD we assume test case has a tree-like structure, for example an AST.

![[Pasted image 20240315101641.png]]
where `DDMIN` is the vanilla delta debugging function.

## Minimality

>[!def] simplify
> Given any 2 trees $T$ and $T'$ 
> 1. we say $T \leadsto T'$  iff $T'$ can be derived from $T$ by _remove 1 and only 1 node_;
> 2. we say $T \leadsto^* T'$ if $T=T'$ or $\exists T'',\; T\leadsto T'' \leadsto^* T'$

>[!def] Global Tree Minimal
> Given program $P$ and tree $T$ then $T'\subset T$ is a _global-tree-minimal_ of $T$ iff 
> 1. $P(T') = \mathrm{fail}$ 
> 2. $\forall T''$ such that $T\leadsto^* T''$ and $P(T'')=\mathrm{fail}$ we have $|T''| \ge |T'|$

>[!thm] 
>Finding _global-tree-minimal_ is _NP-complete_

>[!def] 1-tree-minimal
> Given program $P$ and tree $T$ then $T'\subset T$ is a _1-tree-minimal_ of $T$ iff 
> 1. $P(T') = \mathrm{fail}$ 
> 2. $\forall T''$ such that $T'\leadsto T''$ then we have $P(T'') \neq \mathrm{fail}$. 

>[!thm]  
> In general, HDD is **not** _1-tree-minimal_. 
>
> However, if 
> 1. internal nodes do not count, and
> 2. all leaf nodes are considered on the last level
>
> then HDD is _1-tree-minimal_ w.r.t. leaf elememts. 

Intuitively, after trimming nodes at level $k$, we may result in a state that we are able to remove some nodes at level $k-1$ and still triggers a failure, but _HDD_ do not allow us go back to upper level, _however, this situation will not happen if the program is monotonic_.

**To ensure 1-tree-minimal, we can use HDD* or HDD+**

## HDD+ and HDD*

_HDD+_ consists of loops of 2 phases:
1. run _HDD_ on the tree
2. given _HDD_'s result, use a BFS test remove one node from the result tree.
the loop ends once phase 2 does not find a smaller counterexample

_HDD*_ repetitively calls _HDD_, until the current call does not give a tree smaller than its input.

# References

1. Donaldson, A., Mar 30, D. M. on, & 2021. (2021, March 30). An Overview of Test Case Reduction. _SIGPLAN Blog_. [https://blog.sigplan.org/2021/03/30/an-overview-of-test-case-reduction/](https://blog.sigplan.org/2021/03/30/an-overview-of-test-case-reduction/)
2. Donaldson, A., May 25, D. M. on, & 2021. (2021, May 25). Test Case Reduction: Beyond Bugs. _SIGPLAN Blog_. [https://blog.sigplan.org/2021/05/25/test-case-reduction-beyond-bugs/](https://blog.sigplan.org/2021/05/25/test-case-reduction-beyond-bugs/)
3. Using Delta Debugging: a short tutorial. retrieved from https://www.st.cs.uni-saarland.de/dd/ddusage.php3
4. Misherghi, G., & Su, Z. (2006). HDD: Hierarchical delta debugging. _Proceedings of the 28th International Conference on Software Engineering_, 142–151. [https://doi.org/10.1145/1134285.1134307](https://doi.org/10.1145/1134285.1134307)

#todo #type-theory #dependent-type #Haskell  #type-level-programming

Type Theory is different from Set Theory in the sense of <mark class="hltr-orange">inhabitants of Types is not as natural as elements of Sets.</mark> 

A real analogue of the notion of _subset in set theory does not exist in type theory_ because the association of the type A with a term a of type A is inherent in the sense that <mark class="hltr-orange">if a is a term of type A then it is not a term of another type B.</mark> 


##### Vocabulary (inference/proof rules) of (Dependent) Type Theory

| Judgement                               | Syntax                |
| --------------------------------------- | --------------------- |
| `A` is a type                           | $A: \text{Type}$              |
| `a` is a term of `A`                    | $a: A$                |
| `A` and `B` are equal types             | $A = B : \text{Type}$        |
| `a` and `b` are equal terms of type `T` | $a = b : T$           |
| `P(a)` is a type for every `a: A`       | $a: A \vdash P(a) : \text{Type}$ |
| `f(a): P(a)` for every `a: A`           | $a:A \vdash f(a): P(a)$   |


##### Connection with Logic

| Type Theory             | Logic                           |
| ----------------------- | ------------------------------- |
| $P: \text{Type}_i$      | proposition                     |
| $p: P$                  | evidence/proof                  |
| $D: A\to \text{Type}_i$ | predicate on $A$                |
| $p: \sum_{a:A} D(a)$    | proof of $\exists x\in A, D(X)$    |
| $p: \Pi_{a:A}D(a)$      | evidence of $\forall x\in A, D(x)$ |


# Basic Type Construction

### Dependent Type Family

A dependent type family $D$ a collection of types which alike a function that maps a type to a universe
$$
D: A \to \text{Type}_i
$$
that is 
$$\forall x\in A, D(x):\text{Type}_i$$ 

### Dependent Sum (Dependent Pair Type)

Given a dependent type family $B: A \to \text{Type}_i$, denote the **dependent sum type** as  

$$
\sum_{x:A} B(x)
$$
and the rule

$$
\Gamma \vdash a:A,\; \Gamma\vdash b:B(a)  \over \Gamma\vdash (a, b): \sum_{x:A}B(x)
$$
i.e., the terms of the **dependent sum type** are pairs of $(a, b)$, where $a:A$ and $b:B(a)$.

In Coq's syntax 
$$
  \exists a:A, B(a)
$$

>[!NOTE] Why Dependent Sum is a sum type
>
> the notation of $(a, b)$ can be simplified to $b$, since the term $a$ is intrinsically specified by $b$'s type.
> 
> Thus from the view of _Set Theory_, the _dependent sum_ type is just the _disjoint union_ of the collection of types $\{B(x)| x:A\}$

> [!NOTE] Relation with Product 
> 
> if the type family $B$ is a constant, i.e. $\forall x:A, y:A$ we have $B=B(x)=B(y)$ then we have $$\sum_{x:A} B \equiv A\times B $$


### Dependent Product (dependent function)

A dependent product of dependent type $P$ is denoted by
$$
\prod_{a:A}P(a)
$$
The terms in the dependent production type are **dependent functions** (in Coq's syntax) 
$$f: \forall a: A, P(a)$$  
>[!NOTE] Relation with Exponential object
>
A dependent product can degenerate to a non-dependent product which is exactly the exponential object of $A$ and $B$ 
> $$ \prod_{a:A}B \equiv B^A $$

>[!NOTE] Definition: Section
>
>A _dependent function_ $f$, which is also called a **section** of the dependent family $P$, is just a term in the depend product $\Pi_{a:A}P(a)$


>[!EXAMPLE]
>$$ id: \prod_{A:\text{Type}}(A \to A) $$
>Give any type $A$, $id$ maps to a function from $A$ to $A$ , i.e.,
>$$ id: \forall A:\text{Type}, (A\to A)$$

# The Identity Type


##### Definition

For all type $A: \text{Type}$,  and any term $a: A$ and $a': A$ denote the **identity type** as 
$$
(a=_A a'): \text{Type}
$$
which satisfies that
- (reflexivity) $a =_A a: \text{Type}$ is always inhabited by a term called $\text{refl}_a$, i.e., $$ \text{refl}: \forall x:A, \;x=_A x$$

##### Interpretation

The identity type defines some "equality relation", which need to be proved, on terms.

- (logic) The identity type represents the proposition of $a$ and $a'$ are _propositionally equal_.
- (topology) The identity type represents the set of paths from $a$ to $a'$.


##### Principle of Path-based Induction
Given 
1. term $a:A$, 
2. a type family $$K: \prod_{x:A} \big [ (a=_Ax) \to \text{Type} \big]$$, which associate every path from $a$ to $x$ with a Type.   
3. $$ e: K(a, \text{refl}_a) $$

Then we have some
$$
f: \prod_{x:A}\prod_{p:(x=_Aa)} k(x, p)
$$
such that 
$$
f(a, \text{refl}_a) \equiv e
$$

>[!NOTE] 
>easy to see this principle has an interpretation in topology, if we think the type $a=_Ax$ are paths from $a$ to $x$

>[!EXAMPLE] Example: Dynamic System
> 
> Suppose
>   - $\mathbb N: \text{Type}$ is the type of natural number;
>   - $Q: \text{Type}$, which maybe a set of states;
>   - the type $(Q\to Q): \text{Type}$, which could be thought as transitions between states;
>   - the type $(\mathbb N\to Q): \text{Type}$ can be thought as time series of states or  realisations of some system
>   - Let $f: \mathbb N\to Q$ defined as $$ f(\text{succ}(n)) := c(n, f(n)) $$ where $c: \mathbb N\to Q\to Q$. and $f(0) = s_0$


# Haskell Example

## Existential Type

```haskell
{-# LANGUAGE GADTs #-}
data Some f where 
  Some :: f a -> Some f
```
$$
  \text{Some} = \sum_{a: Type} (\text{f a})
$$

## Dependent Sum

The following Haskell code are from [dependent-sum](https://github.com/obsidiansystems/dependent-sum) package
```haskell
infix 1 :=>, ==>  -- the lowest precedence

DSum :: forall k. (k -> *) -> (k -> *) -> *
data DSum tag f = forall a. !(tag a) :=> f a

(==>) :: Applicative f => tag a -> f a -> DSum tag f
t ==> v = t :=> (pure v)
```
the type `DSum tag f` associates type `tag a` to type `f a`
$$
\forall x\in (\text{tag a}), (\text{DSum tag f})_x \in (\text{f a})
$$
or 
$$
  \text{DSum tag f} = \sum_a (\text{tag a}) \times (\text{f a})
$$

usually `tag a` only have one value and `f == Identity` such as 
```haskell
{-# LANGUAGE GADTs #-}
import Data.Dependent.Sum ( DSum(..) )

data Tag a where 
  TagStr :: Tag String
  TagInt :: Tag Int 
  TagRec :: Tag (DSum Tag Identity)

name = TagStr ==> "Jerrie"
age  = TagInt ==> 34
theGirl = TagRec name ==> age 
```

## Dependent Prod

the package [dependent-map](https://hackage.haskell.org/package/dependent-map) provides a data type `DMap` which is equivalent to 
a partial dependent function/product

# References

- [dependent type theory in nLab (ncatlab.org)](https://ncatlab.org/nlab/show/dependent+type+theory)
- Videos from YouTube
    - [Foundations 7: Dependent Type Theory](https://youtu.be/Wh1QxF5FLJw)
    - [Category Theory For Beginners: Topos Theory And Subobjects](https://youtu.be/o-yBDYgUqZQ)


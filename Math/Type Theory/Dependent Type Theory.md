#todo #type-theory #dependent-type #Haskell  #type-level-programming

Type Theory is different from Set Theory in the sense of <mark class="hltr-orange">inhabitants of Types is not as natural as elements of Sets.</mark> 

A real analogue of the notion of _subset in set theory does not exist in type theory_ because the association of the type A with a term a of type A is inherent in the sense that <mark class="hltr-orange">if a is a term of type A then it is not a term of another type B.</mark> 


## 0.1 Vocabulary (inference/proof rules) of (Dependent) Type Theory

| Judgement                               | Syntax                           |
| --------------------------------------- | -------------------------------- |
| `A` is a type                           | $A: \text{Type}$                 |
| `a` is a term of `A`                    | $a: A$                           |
| `A` and `B` are equal types             | $A = B : \text{Type}$            |
| `a` and `b` are equal terms of type `T` | $a = b : T$                      |
| `P(a)` is a type for every `a: A`       | $a: A \vdash P(a) : \text{Type}$ |
| `f(a): P(a)` for every `a: A`           | $a:A \vdash f(a): P(a)$          |


## 0.2 Connection with Logic

| Type Theory             | Logic                           |
| ----------------------- | ------------------------------- |
| $P: \text{Type}_i$      | proposition                     |
| $p: P$                  | evidence/proof                  |
| $D: A\to \text{Type}_i$ | predicate on $A$                |
| $p: \sum_{a:A} D(a)$    | proof of $\exists x\in A, D(X)$    |
| $p: \Pi_{a:A}D(a)$      | evidence of $\forall x\in A, D(x)$ |

# 1 Theory
## 1.1 Basic Type Construction

### 1.1.1 Dependent Type Family

A dependent type family $D$ a collection of types which alike a function that maps a type to a universe
$$
D: A \to \text{Type}_i
$$
that is 
$$\forall x\in A, D(x):\text{Type}_i$$ 

### 1.1.2 Dependent Sum (Dependent Pair Type)

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

      z`
### 1.1.3 Dependent Product (dependent function)

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


>[!note] Parametric polymorphism 
> parametric polymorphism can be seen as a special case of dependent product.
> ```haskell
> id :: forall a. a -> a
> ```
> is exactly $$ id : \prod_{a:\mathrm{Type}} a $$


>[!def] Section
>
>A _dependent function_ $f$, which is also called a **section** of the dependent family $P$, is just a term in the depend product $\Pi_{a:A}P(a)$


>[!EXAMPLE]
>$$ id: \prod_{A:\text{Type}}(A \to A) $$
>Give any type $A$, $id$ maps to a function from $A$ to $A$ , i.e.,
>$$ id: \forall A:\text{Type}, (A\to A)$$

## 1.2 The Identity Type


### 1.2.1 Definition

For all type $A: \text{Type}$,  and any term $a: A$ and $a': A$ denote the **identity type** as 
$$
(a=_A a'): \text{Type}
$$
which satisfies that
- (reflexivity) $a =_A a: \text{Type}$ is always inhabited by a term called $\text{refl}_a$, i.e., $$ \text{refl}: \forall x:A, \;x=_A x$$

### 1.2.2 Interpretation

The identity type defines some "equality relation", which need to be proved, on terms.

- (logic) The identity type represents the proposition of $a$ and $a'$ are _propositionally equal_.
- (topology) The identity type represents the set of paths from $a$ to $a'$.


### 1.2.3 Principle of Path-based Induction
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


## 1.3 Haskell Examples

### 1.3.1 Existential Type

```haskell
{-# LANGUAGE GADTs #-}
data Some f where 
  Some :: f a -> Some f
```
$$
  \text{Some} = \sum_{a: Type} (\text{f a})
$$

### 1.3.2 Dependent Sum

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

>[!note] 
> existential types is strictly less powerful than dependent-sum. Dependent-sum allows you to retrieve the actual type of the value when unpack it, while existential type cannot.

### 1.3.3 Dependent Prod

the package [dependent-map](https://hackage.haskell.org/package/dependent-map) provides a data type `DMap` which is equivalent to a partial dependent function/product.

# 2 Implementation


```haskell
data Name = Global String   -- ^ bound at toplevel 
          | Local Int       -- ^ bound in a lambda
          | Quote Int
  deriving (Show, Eq)
```

>[!note ] Names & Values:
 > - (_locally bound variables_) represented by [[de Bruijn Index]]. When passing a binder in an algorithm, we have to convert a bound variable into a free variable temporarily, and use Local for that;
 > - (_free variables_) represented using _absolute_ references, i.e. _names_;
 > - (_value_) use _high-order abstract syntax_ to represent values: values that are functions are represented using Haskell functions.

```haskell
{-| A stack can be seen as a list written from left to right,
just like a list can be viewed as written from right to left.
-}
data Stack a = EmptyStack | (Stack a) |: a
  deriving (Show, Eq, Functor, Applicative, Monad)
```

## 2.1 $\lambda_\to$ with bidirectional checking

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeData, PatternSynonyms #-}
type data Direction = Infer | Check
data Term (d :: Direction) where
  ----------------- inferable terms -----------------  
  Ann   :: Term Check -> Ty         -> Term Infer
  Bound :: Int                      -> Term Infer
  Free  :: Name                     -> Term Infer
  (:@:) :: Term Infer -> Term Check -> Term Infer
  ----------------- checkable terms -----------------  
  Inf   :: Term Infer               -> Term Check
  Lam   :: Scope                    -> Term Check
infixl 6 :@:
deriving instance Show (Term d)
deriving instance Eq (Term d)
  

-- | type safety for handling de Bruijn indices
newtype Scope = Sc (Term Check)
  deriving (Show, Eq)

data Ty = TFree Name | Fun Ty Ty
  deriving (Show, Eq)

data Value = VLam (Value -> Value) | VNeutral Neutral
pattern VFree :: Name -> Value
pattern VFree name = VNeutral (NFree name) 

-- | free variables will hold off the reduction process
data Neutral = NFree Name | NApp Neutral Value
```

### 2.1.1 Evaluation

```haskell
type Env = Stack Value

eval :: HasCallStack => Term d -> Env -> Value
eval e env = case e of
  ---------------- inferable ---------------
  Ann   e' _  -> eval e' env
  Bound i     -> env `valueAt` i
  Free  x     -> VFree x
  f :@: x     -> vapp (eval f env) (eval x env)
  ---------------- checkable ---------------
  Inf e'      -> eval e' env
  Lam (Sc e') -> VLam (\x -> eval e' (d |: x))
 where
  valueAt :: HasCallStack => Env -> Int -> Value
  valueAt = \case
    EmptyStack _ -> fail "out of bound"
    (as |: a) i  -> if | i == 0    -> a 
                       | i <  0    -> fail "out of bound"
                       | otherwise -> as `valueAt` (i-1)
  vapp :: Value -> Value -> Value     
  vapp = \case 
    (VLam f) x     -> f x
    (VNeutral n) v -> VNeutral (NApp n v)
```

### 2.1.2 type checking

```haskell 
data Kind = Star deriving (Show)
data Info = HasKind Kind | HasTy Ty 
  deriving (Show)

type Ctx a = [(Name, a)]
type Rst a = Either String a

checkKind  :: Ctx Info -> Ty -> Kind -> Rst ()
inferType' :: Int -> Ctx Info -> Term Infer       -> Rst Ty
checkType' :: Int -> Ctx Info -> Term Check -> Ty -> Rst ()

inferType = inferType' 0
checkType = checkType' 0
```

>[!note] Checksum
>The type-checking functions are parameterized by an integer argument indicating the _number of binders_ we have encountered.

```haskell
checkKind  :: Ctx Info -> Ty -> Kind -> Rst ()
checkKind ctx t kind = case t of
  TFree x -> case lookup x ctx of 
    Just (HasKind k) -> if k == kind 
      then return () 
      else fail "unmatched kind"
    _ -> fail "unknown type identifier" 
  Fun ft at -> do { checkKind ctx ft Star; check ctx at Star}

inferType' :: Int -> Ctx Info -> Term Infer -> Rst Ty
inferType' i ctx = \case 
  Ann e t -> do checkKind ctx t Star 
                checkType i ctx e t 
                return t
  Bound i -> fail "impossible branch"
  Free  x -> case lookup x ctx of 
    Just (HasTy t) -> return t
    _              -> fail "unknown identifier"
  f :@: x -> do 
    ft <- inferType' i ctx f 
    case ft of 
      Fun arg ret -> do checkType' i ctx x arg
                        return ret
      _           -> fail "illegal application"

checkType' :: Int -> Ctx Info -> Term Check -> Ty -> Rst ()
checkType' i ctx e t = case e of 
  Inf e' -> do t' <- inferType' i ctx e'
               unless (t' == t) (fail "type mismatch")
  Lam sc -> case t of 
    Fun arg ret -> let e'   = instantiate sc (Local i)
                       ctx' = ctx |: (Local i, HasType arg)
                   in  checkType' (i+1) ctx' e'
    _ -> fail "type mismatch"
```

>[!note]
>The `{haskell} instantiate` function used in `{haskell}checkType'` is implemented in [[de Bruijn Index#4 Combine name and nameless variables|de Bruijn indices]].

### 2.1.3 Quotation

The `quote` function, which is the right inverse of `eval`, turn a `Value` into a `Term`.

```haskell
quote = quote' 0

quote' :: Int -> Value ->Term Check
quote' i = \case 
  VLam     f -> Lam . Sc $ quote' (i+1) (f (VFree (Quote i)))
  VNeutral n -> Inf $ quoteNeutral i n
 where
  quoteNeutral :: Int -> Neutral -> TermInf
  quoteNeutral i = \case 
    NFree x  -> boundFree i x
    NApp n v -> quoteNeutral i n :@: quote' i v
  boundFree :: Int -> Name -> TermInf
  boundFree i (Quote k) = Bound (i - k - 1)
  boundFree i x = Free x
```

## 2.2 $\lambda_\Pi$

- The _dependent function space_ '$\prod$'^[or denoted as $\forall$ in the paper] generalises the usual function space '$\to$' be allowing the _range_ to be dependent on the _domain_.
- Everything is a term. However, the type judgement `::` must be well formed as:
  1. inhabitable types always have sort $\star$. $$ (\exists e :: t) \implies t::\star $$, which indicates $$\star :: \star$$
### 2.2.1 Term & Value

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeData, 
             PatternSynonyms, LambdaCase, 
             MultiWayIf #-}

data Stack a = EmptyStack 
             | (Stack a) :> a 
  deriving (Show)
infixl :>

data Name = Global String | Local Int | Quote Int 
  deriving (Show, Eq)

type data Direction = Infer | Check

data Term (d :: Direction) where
  ----------------- inferable terms -----------------  
  Ann    :: Term Check -> Term Check -> Term Infer
  -- ^ the 2nd arg is the type of the 1st arg
  --   the type need to be checked against Star
  Bound  :: Int                      -> Term Infer
  Free   :: Name                     -> Term Infer
  (:@:)  :: Term Infer -> Term Check -> Term Infer
  ----------------- checkable terms -----------------  
  Inf    :: Term Infer               -> Term Check
  Lam    :: Scope                    -> Term Check
  ----------------- new terms       ----------------- 
  Star   ::                             Term Infer
  Forall :: Term Check -> Scope      -> Term Infer      
  -- ^ dependent function space
infixl 6 :@:
deriving instance Show (Term d)
deriving instance Eq   (Term d)

-- | type safety for handling de Bruijn indices
newtype Scope = Sc (Term Check)
  deriving (Show, Eq)
  
data Value 
  = VLam (Value -> Value) 
  | VNeutral Neutral
  | VStar 
  | VForall Value            -- ^ dom
            (Value -> Value) -- ^ range
pattern VFree :: Name -> Value
pattern VFree name = VNeutral (NFree name) 

-- | free variables will hold off the reduction process
data Neutral = NFree Name | NApp Neutral Value
```

Term manipulation

```haskell
insta :: Scope -> Term Infer -> Term Check
insta (Sc e) b = replace 0 e  
 where
  replace :: Int -> Term d -> Term d
  replace i o = case o of
    Ann e t            -> Ann (replace i e) t
    Bound j            -> if i == j then b else o
    Free  _            -> o
    f :@: x            -> (replace i f) :@: (replace i x)
    Inf   _            -> o
    Lam   (Sc o')      -> Lam (Sc $ replace (i+1) o')        
    Star               -> o
    Forall dom (Sc o') -> let dom' = replace i dom 
                              rng' = replace (i+1) o'
                          in  Forall dom' (Sc rng')

quote :: Value -> Term Check
quote = go 0
 where 
  go i (VLam f)     = Lam . Sc $ go (i+1) (f (VFree $ Quote i))
  go i (VNeutral n) = case n of 
                        NFree x   -> Inf $ Free x
                        NApp  f x -> 
  go i  VStar       = Inf Star
  goNeutral i 
```

### 2.2.2 Evaluation 

```haskell
type Ctx a = [(Name, a)]
type Env = Stack Value

eval :: Term d -> Env -> Value
eval e env = case e of
  ---------------- checkable ---------------
  Inf e'      -> eval e' env
  Lam (Sc e') -> VLam (\x -> eval e' (env :> x))
  ---------------- inferable ---------------
  Ann   e' _  -> eval e' env
  Bound i     -> env `valueAt` i
  Free  x     -> VFree x
  f :@: x     -> vapp (eval f env) (eval x env)
  ----------------   new    ----------------   
  Star        -> VStar
  Forall d r  -> let Sc rng = r
                     d'     = eval d env
                     r'     = \v -> eval rng (env :> v) 
                 in  VForall d' r'
 where
  valueAt :: Env -> Int -> Value
  valueAt env i = case env of
    EmptyStack -> error "out of bound"
    as :> a    -> if | i == 0    -> a 
                     | i <  0    -> error "out of bound"
                     | otherwise -> as `valueAt` (i-1)
  vapp :: Value -> Value -> Value     
  vapp func x = case func of 
    VLam f        -> f x
    VNeutral n    -> VNeutral (NApp n x)
```


# 3 References

- [dependent type theory in nLab (ncatlab.org)](https://ncatlab.org/nlab/show/dependent+type+theory)
- Videos from YouTube
    - [Foundations 7: Dependent Type Theory](https://youtu.be/Wh1QxF5FLJw)
    - [Category Theory For Beginners: Topos Theory And Subobjects](https://youtu.be/o-yBDYgUqZQ)
- Löh, A., McBride, C., & Swierstra, W. (2010). _A tutorial implementation of a dependently typed lambda calculus_. _102_(2), 177–207.



>[!tldr] 
> _de Bruijn_ notation is a coding of lambda terms in which each occurrence of a bound variable $x$ is replaced by a natural number, indicating **the ‘distance’ from the occurrence to the abstraction that introduced $x$**.

# 1 The Problem of Variable Capture

>[!def] variable capture
> - This phenomenon of _free variables_ in a term $s$ becoming bound when $s$ is naively substituted into a term $t$ is called _variable capture_. To avoid it, we need to make sure that the bound variable names of $t$ are kept distinct from the free variable names of $s$. 
> - A substitution operation that does this correctly is called _capture-avoiding substitution_.

>[!example] variable capture
> $$ [x \mapsto z] (\lambda z . x) = \lambda z . z $$
> the problem here is that the free variable $z$ get bounded on the RHS.

There are many approaches to deal with the viable capture problem, including
1. (**Barendregt Convention**) Introduce a general condition that the names of all bound variables must all be different from each other and from any free variables we may use.
2. We can devise some “canonical” representation of variables and terms that does not require renaming.

>[!warning] Barendregt convention is unstable under substitution
> substitution involves copying the term being substituted, it is easy to construct examples where the result of substitution is a term in which some λ-abstractions have the same bound variable name. 
> This implies that each evaluation step involving substitution must be followed by a step of renaming to restore the invariant.
>
> Consider
> ```haskell
> apply = \f x -> f x
> apply (apply (+1)) = \x -> (\x -> x + 1) x
> ```
> $$ apply(apply(+1)) = \lambda x . (\lambda x . x + 1) x $$

# 2 de Bruijn Index

De Bruijn’s idea was that we can represent terms more straightforwardly—if less readably—by _making variable occurrences point directly to their binders, rather than referring to them by name_. 

This can be accomplished by replacing named variables by natural numbers, where the number $k$ stands for “the variable bound by the $k$'th enclosing $\lambda$.” (i.e. counting bindings from left to right)

>[!def] term & $n$-term
> Let $\mathcal T$ be the smallest family of sets $\{\mathcal T_0, \mathcal T_1, \mathcal T_2, \dots \}$ such that,
> 1. values belongs to $\mathcal T_0$
> 2. $t \in \mathcal T_k \implies t \in \mathcal T_{n+1}$
> 3. if $t_1 \in \mathcal T_n$ and $n > 0$, then $(\lambda.t_1) \in \mathcal T_{n−1}$; 
> 4. if $t_1 \in \mathcal T_n$ and $t_2 \in \mathcal T_n$, then $(t_1 \; t_2) \in \mathcal T_n$.
>
> _The elements of $\mathcal T_n$ (called $n$-terms) are terms with at most $n$ free variables_, which are numbered between $0$ and $n - 1$; a given element of $\mathcal T_n$ need not have free variables with all these numbers, or indeed any free variables at all.


>[!def] de Bruijn Index
> Suppose $x_i$ are variable names, the naming context $\Gamma = x_{n-1},\dots,x_1, x_0$ assigns to each name $x_i$ the _de Bruijn index_ $i$.
> Note that the **rightmost variable** in the sequence is given the index $0$; this matches the way we _count $\lambda$ binders—from right to left—_ when converting a named term to nameless form.


>[!note] $\alpha$-equivalence for free
> the de Bruijn index counted from inner most to outer most abstraction bindings which make $2$ terms $\alpha$ equivalent if and only if they are exactly the same.

```haskell
newtype DeBruijn = DeBruijn Int deriving (Num, Show, Eq)

type family Ctx a where 
  Ctx Debruijn = [Name]

type Checksum = Int

data Term n 
  = TmVar (TmInfo n) Checksum n 
  | TmAbs (TmInfo n) (Ast n)
  | TmApp (TmInfo n) (Ast n) (Ast n)

type family TyInfo n where 
  TyInfo Name = Name
  TyInfo BeBruijn = Void

removeName :: Ctx Debruijn -> Ast Name -> Ast DeBruijn
removeName ctx t = case t of 
  ...
```

## 2.1 Shifting 

> [!def] cutoff parameter $c$
> The `shift` function take a _"cutoff"_ parameter $c$ that controls which variables should be shifted. 
> It starts off from $0$, meaning all variables should be shifted, and gets incremented by one every time the shifting function goes through a binder.


>[!def] shift
> 1. shift variables
 >   $$\uparrow_c^d(k) = \big \{ \begin{align} &k  &k < c \\ &k + d & k \ge c \end{align}$$
> 2. shift abstractions
 >   $$\uparrow_c^d(\lambda . t) = \lambda . \uparrow_{c+1}^d(t)$$
 > 3. shift application
 >   $$\uparrow_c^d(t_1\; t_2) = \uparrow_c^d(t_1)\; \uparrow_c^d(t_2)$$

the reason we need the `shift` is that we need to update the variables names in term $t$ if we substitute free variable $k$ with term $t$.

## 2.2 Substitution

>[!def] substitution
> Let "$[j\mapsto s]t$" denote substitute variable $j$ with term $s$ in term $t$.
> 1. variables
>  $$[j\mapsto s]k = \big \{ \begin{align} &s &k=j \\ &k &k\ne j \end{align}$$
> 2. abstraction
> $$[j\mapsto s](\lambda . t) = \lambda . \; [j+1 \mapsto \; \uparrow^1(s)]t$$
> 3. application
> $$[j\mapsto s](t_1 \; t_2) = ([j\mapsto s]t_1) \; ([j\mapsto s]t_2)$$


# 3 Nested Datatype

We can use _nested datatype_ to guarantee that variables represented by _de Bruijn indices_ are all **bounded variables**, in other words, a $n$-term $t$ do not have de Bruijn index greater or equal to $n$.

```haskell
data Term v = Var v 
            | App (Term v) (Term v)
            | Lam (Term (Incr v)) 
  deriving (Functor)
data Incr v = Zero | Succ !v 
  deriving (Functor)
```

Note that natural number $\mathbb N$ equivalent to `Fix Incr`; and, `Inc (Inc ... (Inc Void) ...)` is equivalent to $\{k \in N : k \le (m-1) \}$ if there are $m$ `Inc`'s in the type. Thus its easy to prove that $\forall n$, de Bruijn indices must less than $n$ in any $n$-term.

>[!note] `Term` as a Monad
> There is a generalisation of de Bruijn notation in which `S`(or `Succ`) can be applied to any term, not just a variable (Paterson, 1991). Its effect is to ***escape the scope of the matching λ***. With this looser representation of terms, one can avoid transforming arguments while substituting.
>f
> The generalised de Bruijn index gives terms with type `Term (Term a)`, and by following the semantics of the generalisation, we can easily write the `join` and `return` for `Term`

```haskell
instance Monad Term where
  return = Var
  ma >>= f = joinTerm (f ma)

joinTerm :: Term (Term a) -> Term a
joinTerm = gFoldTerm id App Lam distTerm
  where 
    distTerm :: Incr (Term a) -> Term (Incr a)
    distTerm Zero = Var Zero
    distTerm (Succ t) = map Succ t

gfoldTerm :: forall m n b 
          . (forall a. m a -> n a) 
          -- ^ for Var
          -> (forall a. n a -> n a -> n a)
          -- ^ for App
          -> (forall a. n (Incr a) -> n a)
          -- ^ for Lam
          -> (forall a. Incr (m a) -> m (Incr a))
          -> Term (m b) -> n b
gfoldTerm v a l k = \case 
  Var x   -> v x
  App f x -> let g = gfoldTerm v a k l 
             in  a (g f) (g x)
  Lam b   -> let b' = map k b :: Term (m (Incr b))
             in  l (gfoldTerm v a l k b' :: n (Incr b))
```

combinators

```haskell
-- make a lambda abstraction 
abstract :: a -> Term a -> Term a
abstract x t = Lam (shift <$> t)
  where 
   shift v = if v == x then Zero else Succ v

apply :: Term a        -- term t
      -> Term (Incr a) -- body of a lambda abstraction
      -> Term a
apply arg =  -- replace every `Zero` in `b` with `t`
    join . map (subst . map Var)
  where 
    subst Zero     = arg 
    subst (Succ v) = v
```

To understand how `apply` is implemented, first adopt the idea of _generalised de Bruijn index_ by just swap in the argument and then use `join`.

# 4 Combine name and nameless variables

```haskell
data Expr = Free Name        -- free variable
          | Bound Int        -- bound variable
          | App Expr Expr    -- application
          | Abs Expr Scope   -- ∀-quantification
  deriving (Show, Eq)
  
newtype Scope = Scope Expr
  deriving (Show, Eq)
```

1. Bound variables are represented using de Bruijn index;
2. The Scope type stands in lieu of the precise ‘term over one more variable’ construction.
3. `Expr` is the type of **closed expressions**—those with _no ‘dangling’ bound variables_ pointing out of scope, and that `Scope` has _**one** dangling bound variable_.
4. The first expression in the `Abs` data constructor represents a _contract_, or precondition of the function.


The `abstract` & `instantiate` function:
1. the `abstract` function replace a _free variable_ with a **dangling** _bounded variable_, which makes a closed `Expr` to a `Scope`.
2. `instantiate` substitute the _dangling free variable_ in the `Scope` with the input expression.

```haskell
-- | The operation abstract name turns a 
--     closed expression (no dangling bound variables)
--     into a scope by turning free variable `Free name` 
--     into the out-most bound variable (de Bruijn index).
abstract :: Name -> Expr -> Scope
abstract name expr = 
  Scope (nameTo 0 expr)
 where
  nameTo :: Int -> Expr -> Expr
  nameTo i = \case 
    Free name'           -> if name' == name 
                              then Bound i 
                              else Free name'
    Bound i'             -> Bound i'
    App f x              -> App (nameTo i f) (nameTo i x)
    Abs dom (Scope body) -> Abs (nameTo i dom) (Scope $ nameTo (i+1) body)

-- | instantiate image turns a `Scope` into an `Expr` 
--     by replacing the out-most de Bruijn index (initially Bound 0) 
--     with image, which we presume is closed.
--
-- N.B. the image parameter must be a closed express, 
--      in other words, all de Bruijn indices in 
--      image are bounded within the image parameter.
instantiate :: Expr -> Scope -> Expr
instantiate image (Sc body) = 
  replace 0 body
 where 
  replace :: Int -> Expr -> Expr
  replace i = \case 
    Free name         -> Free name
    Bound i'          -> if i' == i then image else Bound i'
    App f x           -> App (replace i f) (replace i x)
    Abs dom (Sc body) -> Abs (replace i dom) (Sc $ replace (i+1) body)

substitute :: Expr -> Name -> Expr -> Expr
substitute image name = instantiate image . abstract name
```


>[!note] no need of shift for closed expression
> Notice that  
> 1. in the implementation of `instanciate` we do not shift the `image` argument.  
> 2. there is no need to shift de Bruijn index in argument `image` if it is already bounded within `image`

## 4.1 Hierarchical Name for Free Variables

```haskell
-- | a stack is a reverse ordered list, which makes it more readable
-- pushing elements is to append a new element to the tail
data Stack a 
  = EmptyStack 
  | Stack a |: a   
  deriving (Functor, Applicative, Monad, Show, Eq)

-- 1. The Strings give us legibility; 
-- 2. the Ints an easy way to express uniform sequences 
--   of distinct name-extensions x0, . . . xn.
type Name = Stack (String, Int)

infixl 6 //
(//) :: Name -> String -> Name
root // s = root |: (s, 0)
```

The hierarchical names is used to reflect the hierarchy of _tasks_. Each subtask has a distinct prefix from which to form its names by extension.
- Superiority within the hierarchy of names is just the partial order induced by ‘being a prefix’.
- We say that two names are independent, $xs ⊥ ys$, if neither $xs \le ys$ nor $ys\le xs$.

>[!cite] agents
> In order to work correctly with hierarchical names, the remaining idea we need is to name the _agents_ which _carry out the tasks_, as well as the free variables.
> ```haskell
> type Agency agentT = Name -> agentT
> ```
> The input `Name` is used for the returned `agentT` to choose its own name.

## 4.2 Toolbox

### 4.2.1 Build & decompose abstractions

1. `<--` splits $\lambda$-abstraction to a `Binding` and a _closed_ `Expr`, replacing the outer-most bind-variable with a free variable;
2. `unprefix` runs `<--` as deep as possible and returns a stack of bindings and a closed expression;
3. `-->` is the reverse of `<--`, combining a binding and a closed expression to a $\lambda$ expression;
4. `-->>` is the reverse of `unprefix`.

```haskell
type Dom = Expr

-- | binding represents, res as Name of type Dom 
data Binding = Name ::: Dom
infix 5 :::

-- ================================================================== 
-- split a lambda abstraction
-- ================================================================== 

(<--) :: MonadFail m 
      => Agency (Expr -> m (Binding, Expr))
name <-- expr = case expr of 
  Abs dom sc -> return ( name ::: dom
                       , instantiate (Free name) sc)
  _          -> fail "expression is not a lamdba abstraction"
infix <--
  
-- | repeatively appling `<--` to a lambda abstaction 
-- until to reach a non-abstraction term
-- N.B. `unprefix` is reverse of `-->>` if `root` is 
--      independent of all names in the lambda abstraction
unprefix :: Agency (String -> Expr -> (Stack Binding, Expr))
unprefix root x lambda = go 1 (EmptyStack, lambda)
  where 
   go i (bs, e) = case (root |: (x, i)) <-- e of
     Just (b, e') -> go (i+1) (bs |: b, e') 
     Nothing      -> (bs, e)

-- ================================================================== 
-- assemble a lambda abstraction
-- ================================================================== 

(-->) :: Binding -> Expr -> Expr
(name ::: dom) --> body = Abs dom (abstract name body)
infixr 6 -->

(-->>) :: Stack Binding -> Expr -> Expr 
prefix -->> body = case prefix of 
  EmptyStack -> body
  bs |: b    -> bs -->> (b --> body)
infixr 6 (-->>) 

```


### 4.2.2 Build & decompose application

1. `unapply` decomposes function applications just like `unprefix` decomposes $\lambda$-abstractions;
2. `-$$` applies a function to a stack of arguments while both function and arguments are _free variables_.

```haskell
unapply :: Expr                 -- ^ application term  
        -> (Expr, Stack Expr)   -- ^ (func, args)
unapply e = peel (e, EmptyStack) 
 where 
  peel (App f x, xs) = peel (f, xs |: x)
  peel a@(_, _)      = a


(-$$) :: Name           -- ^ function name
      -> Stack Binding  -- ^ arguments 
      -> Expr
f -$$ args = go (Free f) args
 where 
  go e EmptyStack = e
  go e (bs |: (a ::: _)) = App (go e bs) (Free a) 
infixl 9 -$$
```

### 4.2.3 Generalisation

```haskell
generalise :: Stack Binding 
           -> Binding
           -> (Binding, Expr -> Expr)
generalise bs (name ::: dom) = 
  ( name ::: (bs -->> dom)
  , substitute (name -$$ bs) name )
```

## 4.3 Example: generate inductive elimination

![[Pasted image 20240921094441.png]]

1. targets: the vector to be eliminated;
2. motive: the induction predicate, what to be achieved by elimination;
3. methods: explaining how the motive is to be pursued for each constructor in turn.

### 4.3.1 data types

Given an inductive type $F$, which has data constructors $c$, let $P$ be a induction predicate on $F$
1. type of type constructors: $$F: \forall i_1 : I_1. \; \dots\; \forall i_n:I_n. \; Set$$
2. type of motives: $$P: \forall \vec{i}:\vec{I}.\; \forall x: F(\vec{i}).\; Set$$
3. type of data constructor: $$c: \forall \vec{a}:\vec{A}.\; F(\vec{s})$$
  1. for non-recursive constructors $\vec A$ do not mention $F$;
  2. for recursive constructors, there exists some $\vec{r}: \vec{I}$, such that $$a : \forall \vec{y} : \vec{Y}.\; F (\vec{r})$$ where $\vec{Y}$ does not mention $F$, and the inductive hypothesis has type $$h: \forall \vec{y}: \vec{Y} . P(\vec{r}, a(\vec{y}))$$



```haskell
makeIndElim :: Agency (Binding -> Stack Binding -> Binding)
makeIndElim root (family ::: famtype) constructors = 
  root :::  targets 
       -->> motive 
       -->  (method <$> constructors)
       -->> (bName motive -$$ targets)
 where
  targets = ...
  motive = ...
  method :: Binding -> Binding
  method = ...
```



# 5 References

1. Pierce, B. C. (2002). Chapter 6 Nameless Representation of Terms from book _Types and programming languages_. MIT press.
2. McBride, C., & McKinna, J. (2004). Functional pearl: I am not a number--i am a free variable. _Proceedings of the 2004 ACM SIGPLAN Workshop on Haskell_, 1–9. [https://doi.org/10.1145/1017472.1017477](https://doi.org/10.1145/1017472.1017477)
3. Bird, R. S., & Paterson, R. (1999). De Bruijn notation as a nested datatype. _Journal of Functional Programming_, _9_(1), 77–91. [https://doi.org/10.1017/S0956796899003366](https://doi.org/10.1017/S0956796899003366)

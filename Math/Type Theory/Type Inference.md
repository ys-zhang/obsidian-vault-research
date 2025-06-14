---
paper: SHEARD, T., & PASALIC, E. (2004). Two-level types and parameterized modules. Journal of Functional Programming, 14(5), 547–587. https://doi.org/10.1017/S095679680300488X
---
#Haskell  #Prolog 

# 1 Concepts

## 1.1 PLI

>[!def] Gramma
> A _context-free grammar_ has 4 components: 
> 1. A set of _terminal_ symbols;
> 2. A set of _nonterminals_;
> 3. A set of _productions_ rules;
> 4. A _start_ symbol

>[!def] Syntax-directed rule system
> A _syntax-directed definition_ associates
> 1. with each gramma _symbol_, a set of _attributes_, and
> 2. with each _production rule_, a _semantic rule_ for computing the attributes associated with symbols appearing in the production.
>
> - An attribute is _synthesised_ if its value is determined from the subtree rooted at its symbol; 
> - An attribute is _inherited_ if its value is determined by its ancestors, itself and its siblings.

## 1.2 Language of Inference Rules
 $$
 \begin{array}{lrl}
  \text{Predicate}  & =      &\sigma\sqsubseteq\sigma'\\
                    & \vert\ &\alpha\not\in free(\Gamma)\\
                    & \vert\ &x:\alpha\in \Gamma\\
\\
  \text{Judgment}   & =      &\text{Typing}\\
  \text{Premise}    & =      &\text{Judgment}\ \vert\ \text{Predicate}\\
  \text{Conclusion} & =      &\text{Judgment}\\
\\
  \text{Rule}       & =      &\displaystyle\frac{\textrm{Premise}\ \dots}{\textrm{Conclusion}}\quad [\mathtt{Name}]
\end{array}
 $$


## 1.3 normalisation

>[!def] strong normalisation
> every expression that has a type will terminate computation after a finite number of steps.

the following type system is an example of _strong normalisation_ 

```haskell
data Ty = TyNum | TyArrow Ty Ty
```
In the above setup, we have 
1. the only atom type `TyNum` we not allow infinite value (this is not recursive);
2. the type language only allow finite words.

The proof outline 
1. all types are finite words
2. evaluation must shrink word length (eliminate an arrow)
3. each evaluation step must be finite (TyNum is finite)

# 2 General Method

>[!tldr] 
>First we **generate constraints**, based on program terms, on what the types must be. Then we **solve constraints** to _identify inconsistencies_ and _join together constraints_ spread across the function body

## 2.1 Constraint generation

```haskell
data Ty = ...    -- the language of types

data TyConstraint = TyEq Ty Ty

genTyCon :: Expr -> [TyConstraint]
```

- the `Ty` type are just like _terms_ in [[Prolog]];
- the `TyConstraint` are _clauses_ in [[Prolog]].

To define `Ty` we can ask the question of _"what can we say about the type of an expression?"_
```haskell
data Ty 
  = TyExpr Expr 
  | TyPrim TyPrim 
  | TyArrow Ty Ty
```

>[!note]
> note that the collection of `TyConstraint` can be synthesised bottom up.

# 3 Unification


>[!def] unification
> _Unification_ of $x$ and $y$ is usually defined as finding a _substitution_ $\sigma$ such that 
> $$\sigma(x) = \sigma(y)$$

The following is a reading note of SHEARD[^r1], which uses the [[Lazy Functional State Threads (ST Monad)|ST monad]],  a production level implementation is `unification-fd`[^r2].


## 3.1 An implementation using `ST` monad

```haskell
type Ptr s = STRef s (Maybe (TypeExp s)) 

-- | type parameter `s` represents the thread in `ST` monad
data TypeExp s
  = MutVar (Ptr s) 
  | GenVar Int 
  | OperType String [TypeExp s] 
  
-- | path compression, or pointer chasing
-- note that the returned type expr is either 
-- 1. not a mut var, or
-- 2. a mut var points to Nothing
prune :: TypeExp s -> ST s (TypeExp s) 
prune t = case t of 
  MutVar r -> do 
    m <- readSTRef r 
    case m of 
      Nothing -> return t 
      Just t2 -> do 
        t’ <- prune t2 
        writeSTRef r (Just t’) 
        return t’
  _ -> return t

-- | occurs check
occursInType :: Ptr s -> TypeExp s -> ST s Bool 
occursInType r t = do 
  t’ <- prune t
  case t’ of 
    MutVar r2 -> return (r==r2) 
    GenVar n  -> return False 
    OperType nm ts -> do 
      bs <- mapM (occursInType r) ts 
      return (or bs) 

-- | unification
unifyType :: TypeExp s -> TypeExp s -> ST s ()
unifyType t1 t2 = do 
  t1’ <- prune t1 
  t2’ <- prune t2 
  case (t1’,t2’) of 
    (MutVar r1, MutVar r2) -> 
      if r1==r2 
        then return () 
        else writeSTRef r1 (Just t2’) 
    (MutVar r1, _) -> do 
      b <- occursInType r1 t2’
      if b 
      . then error "occurs in" 
        else writeSTRef r1 (Just t2’) 
    (_,MutVar _) -> unifyType t2’ t1’
    (GenVar n,GenVar m) -> 
      if n==m 
        then return () 
        else error "different genvars" 
    (OperType n1 ts1,OperType n2 ts2) -> 
      if n1==n2 
        then unifyArgs ts1 ts2 
        else error "different constructors" 
    (_,_) -> error "different types" 
 where 
  unifyArgs (x:xs) (y:ys) = do 
    unifyType x y
    unifyArgs xs ys 
  unifyArgs [] [] = return () 
  unifyArgs _ _ = error "different lengths"

instantiate :: [TypeExp a] -> TypeExp a -> TypeExp a 
instantiate ts x = case x of 
  MutVar _ -> x 
  OperType nm xs -> OperType nm (map (instantiate ts) xs) 
  GenVar n -> ts !! n
```

The only none travail thing is the `GenVar` data constructor and the `instantiate` function. The `instantiate` function instantiates a **type template** or **type scheme** where `GenVar` are _slots_ in the _template_.

>[!note] type scheme
>**type schemes** are useful in type-checking _polymorphic expressions_, for instance, the function 
> ```haskell
> lookup :: Int -> [(Int, a)] -> a
> ```
> gives a _type scheme_ with type parameter `a` as a `GenVar`, to understand the intuition just try type-checking the following expression:
> ```haskell
> f :: ? -> ?
> f dict = let a = 1 in a + (lookup a dict)
> ```
`lookup` is type inferred at the call site not the definition site, i.e. 
> 1. at _definition site_, a polymorphic expression _generates_ a *type scheme*,
> 2. at _call site_, a polymorphic expression _initiates_ its *type scheme* for type checking.


## 3.2 Modular implementation



### 3.2.1 Modular Data

The core data type in `unification-df` is a `UTerm`
```haskell
data UTerm t v 
  = UTerm !(t (UTerm t v))
  | UVar !v 
```

`UTerm` is equivalent to the generic term `GT` defined in the paper[^r1].

```haskell
data GT t r 
  = T (t (GT t r))
  | MutVar (r (Maybe (GT t r)))
  | GenVar Int

data T a = OperType String [a]
data Expr s = GT T (STRef s)

-- | implementing GT using UTerm
type GT' t r = UTerm t (Var r t)
data Var' r t 
  = MutVar' (r Maybe (GT' t r) )
  | GenVar' Int 
```

### 3.2.2 Modular Operation

first we examine the necessary primitive operations needed to implement unification.
1. we need `Eq v` and some thing like `Biplate (UTerm t v) v` in `UTerm t v` for _occurrence check_ 
2. we need the ability to unify _sub-terms_ of the same data constructor, which is exactly what the local function `unifyArgs` do in the function `unify`

#### 3.2.2.1 Term structure

The 2nd requirement gives the `unification-fd` typeclass `Unifiable` and the `matchS` method in the paper[^r1]

```haskell
-- | `unification-fd`
-- the `t` in `UTerm t v` must implement this `Unifiable`
class Traversible t => Unifiable t where
    zipMatch :: t a -> t a -> Maybe (t (Either a (a, a)))
--  matchS   :: t a -> t a -> Maybe  [           (a, a)]
```

the `Left` branch of `Either x (x, x)` means we do not need to inspect this sub term further, _it’s already unified_.

```haskell
matchS’ :: Unifiable t => t a -> t a -> Maybe [(a, a)]
matchS’ t1 t2 = zipMatch t1 t2 
  <&> toList  -- Foldable f :: f a -> [a]
  <&> rights  -- [Either a b] -> [b]
```

>[!note] semantics of unification
> in logic programming, the semantics of a _unification of a pair of terms succeeds_ is some first order logic constraint is _"solvable"_. And what _solvable_ means is determined by the log and the constraint.
>
> consider the following semantics:
> 1. (_structural equality_) there is a substitution makes the 2 terms exactly same
> 2. (_partial order_) there is a substitution makes one term _"less than"_ the other.
>
> the 1st one will only use the `Left (a, a)` case
> the 2nd one will probably use the `Right a` case when _some sub-term exists in one term but not the other_.

#### 3.2.2.2 Term Variable

```haskell
class Eq v => Variable v where
  -- unique identifier
  getVarID :: v -> Int 

class (Unifiable t, Variable v, Monad m) 
    => BindingMonad t v m | m t -> v, m v -> t
    where
  lookupVar :: v -> m (Maybe (UTerm t v))
  -- | new free var
  -- `freeVar >>= lookupVar === pure Nothing`
  freeVar :: m v
  -- | new binded var 
  newVar :: UTerm t v -> m v
  newVar t = do 
    v <- freeVar 
    bindVar v t
    return v
  bindVar :: v -> t -> m ()
```

### 3.2.3 implement unify

```haskell
prune :: BindingMonad t v m 
      => UTerm t v 
      -> m (UTerm t v)
prune t = case t of 
  UVar v -> do 
    mbt' <- lookupVar v
    case mbt' of 
      Nothing -> return t
      Just t' -> do 
        t'' <- prune t'
        bind v t''
        return t''
  _ -> t

occursInTerm :: v -> Uterm t v -> m Bool
occursInTerm v t = do 
  t0 <- prune t
  case t0 of 
    UVar v' -> 
      return (v == v')
    UTerm t' -> do
      os <- mapM (occursInTerm v) t'
      return (or os)

unifyTerm :: BindingMonad t v m
          => UTerm t v 
          -> UTerm t v 
          -> m ()
unifyTerm t1 t2 = do 
  t1' <- prune t1
  t2' <- prune t2
  case (t1', t2') of 
    (UVar v1, UVar v2) ->  
      if v1 == v2 
        then return ()
        else bindVar v1 t2'
    (UVar v1, _) -> do 
      b <- occursInTerm v1 t2'
      if b 
        then error "occurs in"
        else bind v1 t2'
    (_, UVar _) -> unifyTerm t2' t1'
    (UTerm t1'', UTerm t2'') -> 
      case zipMatch t1'' t2'' of 
        Nothing -> error "different term stucture"
        Just xs -> 
          sequence_ 
            [ unifyTerm u1 u2 
            | (u1, u2) <- rights xs
            ]
```

## 3.3 package `unification-fd`

We list primitive of the library.

> `fd` for finite domain, similar to the famous prolog package [clp(fd)](https://www.swi-prolog.org/man/clpfd.html)

### 3.3.1 Term mutability 

The `UTerm t v` represents a **mutable** term which can be changed during _unification_.

we have primitive to get an _immutable_ version

```haskell
type Fix :: (* -> *) -> *
data Fix f = Fix { unfix :: f (Fix f) }

-- | freeze will succeed iff the term 
-- does not contain variable
freeze :: Traversible t => UTerm t v -> Maybe (Fix t)
unfreeze :: Functor t => Fix t -> UTerm t v

-- | this function substitute binded variable with
-- their bindings
applyBindings 
  :: (BindingMonad t v m, MonadTrans em,...)
  => UTerm t v
  -> em m (UTerm t v)
```

conceptually `freeze` cannot succeed if the term contains any _unbounded variable_. Additionally, since the function `freeze` is pure^[there is no monad in its type], it will fail even though it contains a bounded variable.


### 3.3.2 Type Scheme (Term templates)

term templates can be achieved using the `freshen/freshenAll` primitive

```haskell
freshen :: (BindingMonad t v m, MonadTrans em, ...)
        => UTerm t v -> em m (UTerm t v)
        
freshenAll 
  :: (Traversable s, BindingMonad t v m, MonadTrans em, ...)
  => s (UTerm t v) 
  -> em m (s (UTerm t v))

getFreeVars :: BindingMonad t v m 
            => UTerm t v
            -> m [v]
```

- `freshen` is equivalent to the _template instantiate_ function
- the argument of the the `freshen` function is the _template_, with all variables treated as `GenVar`
- `freshen` first gather all variables^[no matter bound or free] of the template, then make a substitution each of them to a new/fresh variable.

### 3.3.3 Term operation

#### 3.3.3.1 Structural Equality

```haskell
equals :: BindingMonad t v m 
       => UTerm t v m 
       -> UTerm t v m
       -> m Bool
       
equiv :: (BindingMonad t v m, MonadTrans em, ...)
       => UTerm t v m 
       -> UTerm t v m
       -> em m (Maybe (IntMap Int))
       -- ^ returns the alpha transform,
       -- map from varID of LHS to varID of RHS

(===) = equals
(=~=) = equiv
```

>[!warning] $\alpha$-transform
> the difference of `equals` and `equiv` is whether modulo $\alpha$-transforms.
> - `equals` treats $\alpha$ transform _unequal_
> - `equiv` treats $\alpha$ transform _equal_

#### 3.3.3.2 unification

```haskell
unify :: (BindingMonad m, MonadTrans em, ...) 
      => UTerm t v 
      -> UTerm t v
      -> em m (UTerm t b)
-- | a function do occors-check strictly
unifyOccurs 
  :: (BindingMonad m, MonadTrans em, ...) 
  => UTerm t v 
  -> UTerm t v
  -> em m (UTerm t b)

subsumes
  :: (BindingMonad m, MonadTrans em, ...) 
  => UTerm t v 
  -> UTerm t v
  -> em m Bool

(=:=) = unify
(<:=) = subsumes
```

>[!def] subsume
> a term $t_1$ **subsumes** a term $t_2$ iff there exists a substitution $\sigma$ such that 
> $$ \sigma (t_1) = t_2 $$ 


[^r1]: SHEARD, T., & PASALIC, E. (2004). Two-level types and parameterized modules. _Journal of Functional Programming_, _14_(5), 547–587. [https://doi.org/10.1017/S095679680300488X](https://doi.org/10.1017/S095679680300488X)
[^r2]: [Hackage: unification-fd](https://hackage.haskell.org/package/unification-fd )
[^r3]: [Tutorial of unification-fd](https://winterkoninkje.dreamwidth.org/100478.html) 

# 4 Hindley-Milner

## 4.1 Concepts

Types are separated into 2 groups, _monotypes_ and _polytypes_

```haskell
newtype TyName = TyName String
newtype TyVarName = TyVarName String

data Ty
  = TyPoly Var TyPoly  
  -- ^ this is exactly `forall var . t`
  | TyMono TyMono
  
data TyMono 
  = TyConstr Int TyName
  | TyApp TyMono TyMono
  | TyVar Var

```

>[!tip] RankNTypes
> notice that type `Ty` do not allow _application_ of polymorphic types which is exactly what `RankNTypes` enables.

[^r4]: [cs4410 type checking](https://course.ccs.neu.edu/cs4410sp19/lec_type-checking_notes.html)
[^r5]: [cs4410 type inference](https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html)


# 5 Occurrence Typing

See [flow sensitive typing (occurrence typing)](https://en.wikipedia.org/wiki/Flow-sensitive_typing)

In statically typed languages, the type of an expression is determined solely by its subexpressions, in other words, typing is a synthesis process.

However, in _flow sensitive typing_, the context of an expression also has a word on the type of the expression. Usually its narrows down the synthesised type, which is inferred from its subexpressions.

## 5.1 Examples 

narrowing in typescript
```ts
function padLeft(
    padding: number | string,
    input: string
): string 
{
  if (typeof padding === "number") {
    return " ".repeat(padding) + input;
  }
  return padding + input;
}```

```lisp
(: flexible-length (-> (U String (Listof Any)) Integer))
(define (flexible-length str-or-lst)
  (if (string? str-or-lst)
      (string-length str-or-lst)
      (length str-or-lst)))
```
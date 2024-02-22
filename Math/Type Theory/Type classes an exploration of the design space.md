#academic-paper #type-theory 

# Some Notes or Concepts

## Cases of multi-parameter type classes

There are 3 types of cases when multi-parameter type-class is necessary:

1. (_Overloading with coupled parameters_): the class signature is really over a tuple of types and where instance declarations capture direct relationships between specific tuples of type constructors.
2. (_Overloading with constrained parameters_): make instance declarations that constrain the element type on a per-instance basis.
3. (_Type relations_).


```haskell
-- case 1: overloading with couple parameters
--   type parameters are more "tightly" entwined
class Monad m => VarMonad m v where
  new :: a -> m (v a) --       <--- `m` and `v` are coupled  
  get :: v a -> m a
  put :: v a -> a -> m ()

-- case 2: overloading with constrained parameters
--   if the type var `a` is universally quantified in 
--   each method then all `Container` instances must
--   share the same constraint on their element.
class Container c a where
  empty  :: c a
  insert :: a   -> c a -> c a
  union  :: c a -> c a -> c a

-- case 3: type relation
--   represents a types relatioin  
class Iso a b where 
  iso :: a -> b
  osi :: b -> a

```


>[!NOTE]
>In some cases, especially the 3rd case (type relation), some type var does not show up in the return type of some method, its difficult for the compiler to infer the missing type variable. 
>
>```haskell
>new :: VarMonad m v => v a -> m a 
>iso :: Iso a b      => a   -> b
>```
> we can use [`FunctionalDependencies`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/functional_dependencies.html#) to mitigate the problem

## Context reduction & Generalisation

_Context reduction_ is the process to simplify the context, it only considers the LHS of ` => ` and has nothing to do with the RHS.

_Generalisation_, on the other hand, is about assign the most general type to a variable, which relates to both LHS and RHS.

The result of generalisation depends on how much context reduction is done before generalisation. Intuitively, this is due to the fact that context reduction is not reversible process (or equivalent relation) in general.

## Overlapping Instances

>[!NOTE] Overlapping
> `instance P1 => Q1` and `instance P2 => Q2` are _overlapping_ if and only if `Q1` and `Q2` are _unifiable_.


```haskell
class MyShow a where
  myShow :: a -> String

instance MyShow a => MyShow [a] where
  myShow = myShow1
  
instance MyShow [Char] where
  myShow = myShow2

-- which function is called in `f "c"`, `myShow1` or `myShow2`?
let f x = myShow (x ++ x)
in  (f "c", f [True, False])
```

if `f` has the type
- `MyShow a => [a] -> String` then `myShow1` is used 
- `MyShow [a] => [a] -> String` then `myShow2` is used

the problem here is that `f` is very likely to be inlined, and inlining shall not change the dynamic semantics.

# References

- Jones, S. P., Jones, M., & Meijer, E. (1997). Type classes: An exploration of the design space. _Haskell Workshop_, 1–16.
- [Comparing Traits and Type-classes](https://terbium.io/2021/02/traits-typeclasses/)

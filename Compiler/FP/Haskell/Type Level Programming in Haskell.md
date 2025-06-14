#Haskell #type-level-programming 


# The Kind System and GHC Extensions

>[!NOTE] Types and Kinds are not essentially different
> GHC 8 extends the idea of kind polymorphism by declaring that _**types and kinds are indeed one and the same**_. 
> 
> Nothing within GHC distinguishes between types and kinds.
>
> This lack of distinction between types and kinds is a hallmark of dependently typed languages.
>
> Axiom : `Type :: Type` or 
> ```
> λ> import Data.Kind (Type)
> λ> :kind Type 
> Type :: *
> λ> import GHC.TypeLits (Nat, Symbol)
> λ> :kind Nat
> Nat :: *
> λ> :kind Symbol 
> Symbol :: *
> ```

## Caveats 

- _type families_ must be saturated;
- Unlike _type families_, which must be saturated, _type constructor_ is curried;
- the kind `Type` or `*` only contains _boxed/lifted_ types;
- Only kind `Type` can have inhabitable types, when unboxed types are not considered;
- type synonym are also promoted as kind synonym 

  ```haskell
  {-# LANGUAGE DataKinds #-}
  import Data.Kind (Type)
  
  -- promotion of type synonym
  type TyFun :: Type -> Type -> Type
  -- ^ kind sig of the type cons `TyFun`
  data TyFun :: Type -> Type -> Type
  
  type (~>) :: Type -> Type -> Type 
  -- ^ kind sig of the type cons `~>`
  type a ~> b = TyFun a b -> Type
  -- ^ at type level: 
  --      `someVar :: a ~> b` is just `someVar :: TyFun a b`
  --   at kind level:
  --     `sometype :: k1 ~> k2` is just `sometype :: TyFun k1 k2 -> Type`
  ```
  
  - at type level,
       `someVar :: a ~> b` is just `someVar :: TyFun a b`
  - at kind level,
       `sometype :: k1 ~> k2` is just `sometype :: TyFun k1 k2 -> Type`
- `data T :: Type -> Type` is "promoted" to a "kind constructor" which `forall t :: Type` takes `t` to create a type of kind `T t` 


## PolyKinds

```haskell
data App f a = MkApp (f a)
```

Kind of the type constructor `App`:
- (`-XNoPolyKinds`):  `App :: (* -> *) -> * -> *`
- (`-XPolyKinds`):  `App :: forall k . (k -> *) -> k -> *`

Kind Inference Rules under `PolyKinds`: 
1. _(right-hand side)_: GHC infers the most polymorphic kind consistent with the right-hand side. 
  - `class`
  - `GADTs`
2. _(no right hand side)_, GHC defaults argument and result kinds to `Type`, except when directed otherwise by a kind signature.


# References

- [GHC Manual (Kind Polymorphism)](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html)
- [GHC Manual (Datatype Promotion)](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#)
- [Blog: Haskell's kind system a primer](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)
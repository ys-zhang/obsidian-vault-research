A _representable functor_ is a functor that isomorphic to a hom-functor $\mathbf{Hom}(a, -)$ .

```haskell
class Representable f where
  type Rep f :: *  -- the represent object "a"
  tabulate :: (Rep f -> x) -> f x
  index :: f x -> Rep f -> x
```
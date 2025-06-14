#category-theory 

A _representable functor_ is a functor that isomorphic to the Hom-functor $\mathbf{Hom}(a, -)$ .

```haskell
class Representable f where
  type Rep f :: *  -- the represent object "a"
  tabulate   :: (Rep f -> x) -> f x
  index      :: f x -> Rep f -> x
  
rule_iso = tabulate . index === id
             && index . tabulate === id
```

Its easy to see the isomorphism when noticing 
1. $a = Rep\; f$;
2. $tabulate : (a \to x) \to f_x$;
3. $index : f_x \to (a\to x)$

# The adjunctions package

```haskell
class Functor g => Distributive g where
  distribute :: Functor f => f (g a) -> g (f a)  
  collect    :: Functor f => (a -> g b) -> f a -> g (f b)
  
class Distributive f => Representable f where
  type Rep f :: *  
  tabulate   :: (Rep f -> x) -> f x
  index      :: f x -> Rep f -> x
```


# References

1. [Hackage-adjunctions](https://hackage.haskell.org/package/adjunctions-4.4.2/docs/Data-Functor-Rep.html#t:Representable)
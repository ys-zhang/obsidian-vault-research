#category-theory 


>[!def] f-algebra
> An **F-algebra** is a triple consisting of 
>
> 1. an _endofunctor_ 𝐹
> 2. (**carrier**): an _object_ 𝑎, 
> 3. (__evaluator__):  a morphism $F \; a \to a$.
> 
> The functor $F$ together the evaluator defines the inner structure of the target object $F\; a$.


```haskell
{-# LANGUAGE RankNTypes #-}

-- Control.Functor.Algebra  in category-extras
type Algebra f a = f a -> a

prop_falgebra :: forall f a 
              .  Eq a
              => Algebra f a
              -> (a -> b) 
              -> f a 
              -> Bool
prop_falgebra eval g fa = 
  eval (fmap g fa) == g (eval fa)
```

For example, a Ring can be defined as 

```haskell
type RingF a = Zero 
             | One 
             | Add a a
             | Mul a a
             | Neg a
```
we can have a F-algebra with $\mathbb{Z}$ as its _carrier_ by giving it an _evaluator_:
```haskell
evalZ :: F Int -> Int
evalZ Zero = 0
evalZ One  = 1
...
```

# 1 F-algebra as a category

 Given an endofunctor $F$, the objects $(F, a, e)$ forms a category of F-algebra generated by $F$, where morphisms over $(F, a, e_a)$ and $(F, b, e_b)$ are functions $f : a \to b$, such that $f \circ e_a = e_b \circ F(f)$
 
 The _initial object_ of this category is the _fix point_ of $F$, i.e. $(F, \mu_F, e_{\mu_F})$, and the morphism is the function $cata(e_a) :: \mu_F \to a$ 

```haskell
data Fix f = Fix {unFix :: f (Fix f)}

eval :: Algebra f (Fix f)
--      f (Fix f) -> Fix f
eval = Fix
```


# 2 Catamorphism

> [!tldr]
> Catamorphisms are generalisations of the concept of a **fold** in functional programming. A *catamorphism* deconstructs a data structure with an *F-algebra* for its underlying functor.

given the _initial object_ $i=Fix(f)$, for any F-algebra (object in the category), we have a morphism $m$ such that
![[Pasted image 20240708153807.png|center]]

Note that $f = eval$ and $m=cata(eval)$ in the diagram.

```haskell
cata :: Functor f => Algebra f a -> Fix f -> a
--      Functor f => (f a -> a)  -> Fix f -> a
cata eval = eval . fmap (cata eval) . unFix
```

## 2.1 Generalised catamorphism



# 3 References

1. [Understanding F-Algebras](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras)
2. [Haskell Wiki: catamorphisms](https://wiki.haskell.org/Catamorphisms)


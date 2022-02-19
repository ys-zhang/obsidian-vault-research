An **F-algebra** is a triple consisting of 
1. an _endofunctor_ 𝐹
2. (**carrier**): an _object_ 𝑎, 
3. (__evaluator__):  a morphism $F \; a \to a$.

The functor $F$ together the evaluator defines the inner structure of the target object $F\; a$.

For example a Ring can be defined as 
```haskell
type RingF a = Zero 
             | One 
             | Add a a
             | Mul a a
             | Neg a

evalZ :: F Int -> Int
evalZ Zero = 0
evalZ One  = 1
...
```


**All F-algebras over a specific endofunctor $F$ forms a category**.

![[Pasted image 20220219104337.png]]

**The initial object in the F-algebra in **

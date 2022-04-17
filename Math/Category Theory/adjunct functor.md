![[Pasted image 20220214203335.png]]

We have category $\mathscr C$ and $\mathscr D$ and two functors, _left_ and _right_:

$$
\begin{align}
L &: D \to C \\
R &: C \to D
\end{align}
$$
satisfies and two natural transformation _unit_ and _counit_,

$$
\begin{align}
\eta &: I_{\mathscr D} \to R \circ L \\
\varepsilon &: L \circ R \to I_{\mathscr C} 
\end{align}
$$

>[!NOTE]
>Another way of looking at unit and counit is that 
> 1. _Unit_ lets us introduce the composition $R\circ L$ anywhere we could insert an identity functor on $\mathscr D$; 
> 2. _Counit_ lets us eliminate the composition $L\circ R$, replacing it with the identity on $\mathscr C$.


![[Pasted image 20220214204339.png]]

$$
\begin{align}
\varepsilon_{Ld} \circ L(\eta_d) &= id_{Ld} \\
R(\varepsilon_{c}) \circ \eta_{Rc} &= id_c
\end{align}
$$

# Another definition

 ![[Pasted image 20220214225617.png]]
We say that $L$ is left adjoint to $R$ iff. there is an isomorphism of hom-set

$$
\mathbf C(Ld, c) \cong \mathbf D(d, Rc)
$$
In detail, there exists natural transformation between 
$$
\begin{align}
c &\to \mathbf C(Ld, c) \\
c &\to \mathbf D(d, Rc)
\end{align}
$$
and another natural transformation between
$$
\begin{align}
d &\to \mathbf C(Ld, c) \\
d &\to \mathbf D(d, Rc)
\end{align}
$$


# Haskell 

_unit_ and _counit_ is defined as `return` and `extract` in Haskell
```haskell
return  ::   d -> m d -- m is the endofunctor of R . L
-- f :: a -> b
-- a -> m b 
(fmap f) . return = return . f

extract :: w c -> c   -- w is the endofunctor of L . R
-- f :: a -> b
-- w a -> b
extract . (fmap f) = f . extract 
```

In Haskell the typeclass `Adjunction` defines the  adjunction relationship of two endo-functors.

```haskell
-- `u` is the right
-- `f` is the left
class (Functor f, Representable u) => 
       Adjunction f u | f -> u, u -> f where

  unit   ::       a -> u (f a) 
  counit :: f (u a) -> a
  leftAdjunct  :: (f a -> b) -> (a-> u b) 
  rightAdjunct :: (a -> u b) -> (f a -> b)
  
  unit = leftAdjunct id 
  counit = rightAdjunct id 
  leftAdjunct f = fmap f . unit 
  rightAdjunct f = counit . fmap f
```
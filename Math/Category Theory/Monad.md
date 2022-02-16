# Algebraic Interpretation
A **monad** is defined as an _endofunctor_ $T$ equipped with _a pair of natural transformations_ $\mu$ (join/multiplication) and $\eta$ (return/unit).

$$
\begin{align}
\mu &: T^2 \to T \\
\eta &: I \to T
\end{align}
$$
**Monadic rules**:
![[Pasted image 20220215224846.png]]

$$
\mu \circ \mu T = \mu \circ T\mu
$$

![[Pasted image 20220215230228.png]]

$$
\mu \circ T\eta = \mu\circ \eta T = 1_T
$$


_The first rule indicates that Kleisli fish arrow is associative_:
```haskell
h <=< (g <=< f) = (h <=< g) <=< f
```
![[Pasted image 20220216003031.png]] 

$$
\begin{align}
& h \leftarrowtail (g \leftarrowtail f) \\
=\;& \mu_{d} \circ Th \circ  \mu_c \circ Tg \circ f \\
=\;& \mu_{d} \circ (Th \circ  \mu_c) \circ Tg \circ f\\
=\;& \mu_{d} \circ (\mu_{Td} \circ  T^2h) \circ Tg \circ f\\
=\;& (\mu_{d} \circ \mu_{Td}) \circ  T^2h \circ Tg \circ f\\
=\;& (\mu_{d} \circ T\mu_d) \circ  T^2h \circ Tg \circ f\\
=\;& \mu_{d} \circ (T\mu_d \circ  T^2h \circ Tg) \circ f\\
=\;& \mu_{d} \circ T(\mu_d \circ  Th \circ g) \circ f\\
=\;& \mu_{d} \circ T(h \leftarrowtail g) \circ f\\
=\;& (h \leftarrowtail g) \leftarrowtail f
\end{align}
$$

_The second rule is says that __unit__ is __almost__ the identity morphism in Kleisli category_:
1. $\eta$ is the left identity, i.e. $\forall f: C\to TC,\; \eta \leftarrowtail f = f$;
2. $\eta$ is the right identity in $TC\subset C$, i.e. $\forall f: TC \to T(TC),\; f\leftarrowtail \eta = f$

# Monoidal Category

A _tensor product_ is a bifunctor that is associative up to natural isomorphism.
A _monoidal category_ is a category with :
1. a _tensor product_,
2. a special object $i$ called _unit_,
3. and 3 natural isomorphisms
$$
\begin{align}
\alpha|_{abc} &: (a\otimes b) \otimes c \to a\otimes (b \otimes c) \\
\lambda_a &: i \times a \to a \\
\rho_a &: a \times i \to a \\
\end{align}
$$

> All told, monad is just a monoid in the category of endofunctors.

# Example


## The Continue Monad
```haskell
-- Control.Monad.Trans.Cont
type Cont r a = (a -> r) -> r

instance Monad (Cont r) where
  return :: a -> Cont r a
  return a = \f -> f a
  >>= :: ((a -> r) -> r) ->
         (a -> (b -> r) -> r) ->
         ((b -> r) -> r)
  ka >>= akb = \rb -> let ra = ( flip akb ) rb
                      in ka ra 
```

![[The Continue Monad.excalidraw]]

```ad-note
As you can see, continuations are composed _inside out_.
The final handler `rb` is called from the innermost layer of the computation.
```

## IO Monad

There is no equivalent of `runState` or `runReader` for the `IO` monad. There is no `runIO`! As long as there was no way of extracting the character from this container, we could claim that the function is pure.

It’s just like the box with the Schrödinger’s cat inside — except that there is no way to open or peek inside the box.

In Haskell, `IO` is a monad. It means that you are able to compose Kleisli arrows that produce `IO` objects.

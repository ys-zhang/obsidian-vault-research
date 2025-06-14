#category-theory  #functional-programming 

 
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
\begin{align}
  \mu \circ \mu T = \mu \circ T\mu
\end{align}
$$

$$
\begin{align}
  \mu_a : T^2 a \to T a \\
  (\mu \circ T)_a = \mu_{Ta}\\
  (T \circ \mu)_a = T (\mu_a)
\end{align}
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


# Monoid 

## Monoidal Category

TLDR;
> All told, monad is just a monoid in the category of endofunctors.


A _tensor product_ is a bifunctor that is associative up to natural isomorphism.
A __monoidal category__ is a category with :
1. a _tensor product_,
2. a special _object_ $i$ called _unit_,
3. and _natural isomorphisms_ (associator, left unitor, right unitor):
    $$
    \begin{align}
    \otimes &: \mathbf{C} \times \mathbf{C} \to \mathbf{C} \\
    \alpha_{abc} &: (a\otimes b) \otimes c \to a\otimes (b \otimes c) \\
    \lambda_a &: i \otimes a \to a \\
    \rho_a &: a \otimes i \to a \\
    \end{align}
  $$


> [!NOTE]
> Easy to see the objects in monoidal category have a monoid structure (up to isomorphism), the difference with the original [[Monoid]] definition is that the objects may not form a Set


## Define monoid through Monoidal Category

The problems is how to define a monoid structure inside an object $m$ without peek in to $m$. 
To define a monoid we need a _multiply_ defined on $m$ and an identity in $m$:
$$
  \begin{align}
    \mu &: m \otimes m \to m \\
    \eta &: i \to m
  \end{align}
$$
and following rules:
1. unit rule:
  $$
    \begin{align}
      \mu \circ (\eta \otimes id) = \lambda_m\\  
      \mu \circ (id \otimes \eta) = \rho_m
    \end{align}
  $$
  ![[Pasted image 20220217132055.png]]
1. associativity rule:
  $$
  \mu \circ (\mu \otimes id) = \mu \circ (id \otimes \mu) \circ \alpha 
  $$
  ![[Pasted image 20220217132029.png]]

The category of endofunctors is a monoidal category:
1. objects are endofunctors
2. morphisms are natural transformations
3. functor composition serves as tensor product.
        functor composition is indeed a bifunctor (see [[Category Theory#Horizontal Composition |horizontal composition]])
        let $F, G, F', G' \in \mathscr F$ and $\alpha : F \to F', \; \beta: G \to G'$.
$$
\begin{align}
&\beta \otimes \alpha \; : \; G\circ F = G\otimes F \to G' \otimes F' = G'\circ F' \\
(&\beta \otimes \alpha)_a 
  = \beta_{F'a} \circ G(\alpha_a)
  = G'(\alpha_a) \circ \beta_{Fa}
\end{align}
$$

# Comonad

> [!NOTE]
> A comonad, on the other hand, provides the means of extracting a single value from it. It does not give the means to insert values. 
> So if you want to think of a comonad as a container, it always comes _pre-filled with contents_, and it lets you peek at it.
> 
> Just as a Kleisli arrow takes a value and produces some embellished result — it embellishes it with context — a co-Kleisli arrow takes a value together with a whole context and produces a result.


The intuition behind these functions is based on the idea that, in general, a comonad can be thought of as a container filled with values of type a (the product comonad was a special case of just one value). There is a notion of the “current” value, one that’s easily accessible through extract. 

> A co-Kleisli arrow performs some computation that is focused on the current value, but it has access to all the surrounding values.


```haskell
class Functor w => Comonad w where 
  extract :: w a -> a
  -- ^ dual of return :: a -> m a
  duplicate :: w a -> w (w a)
  -- ^ dual of join :: m (m a) -> m a
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b 
  -- ^ dual of bind :: m a -> (a -> m b) -> m b
  extend f = fmap f . duplicate
  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)

```

## Comonoid 

```haskell
class Comonoid m where 
  split   :: m -> (m, m)
  destroy :: m -> ()
```

Comonoid Rules:

```haskell
id == fst . bimap id destroy . split 
id == snd . bimap destroy id . split
```

## Store/Costate Comonad

```haskell
data Store s a = Store (s -> a) s

instance Comonad (Store s) where 
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
```


# Equip with a F-algebra

A monad can be equip with a [[F-algebra]].
let $\eta$ and $\mu$ are the _unit_ and _multiplier_ of the monad $T$ 
let $alg$ be the algebraic evaluator
1. The intuition is that $\eta_a$ creates a trivial expression from a value of type $a$;
2. evaluation of $m \circ m \; a$ is compatible with the multiplier.

$$
\begin{align}
alg \circ \eta &= id \\
alg \circ \mu &= alg \circ T(alg)
\end{align}
$$

![[Pasted image 20220219164156.png]]





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

![[The Continue Monad.excalidraw|700]]

> [!NOTE]
> As you can see, continuations are composed _inside out_.
> The final handler `rb` is called from the innermost layer of the computation.


## IO Monad

There is no equivalent of `runState` or `runReader` for the `IO` monad. There is no `runIO`! As long as there was no way of extracting the character from this container, we could claim that the function is pure.

It’s just like the box with the Schrödinger’s cat inside — except that there is no way to open or peek inside the box.

In Haskell, `IO` is a monad. It means that you are able to compose Kleisli arrows that produce `IO` objects.


## The Stream Comonad

A stream is just like a list, except that it doesn’t have the empty constructor



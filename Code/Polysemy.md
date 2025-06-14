#category-theory 

Some of the type definitions

```haskell
-- | the Effect kind
type Effect = (Type -> Type) -> Type -> Type
type InterpreterFor e r = forall a . Sem (e:r) a -> Sem r a
```

# 1 High order effects and `Final` effect

>[!def] high order effect
> _Higher-order effects_ denotes an effect constructor that uses the monad `m` in its parameters, allowing it to store an entire region for evaluation in an interpreter.

One example of higher-order effect is the `Resource` effect

```haskell
-- defined in `Polysemy.Resource`

data Resource m a where
  Bracket :: m a          -- action to aquire the resource
          -> (a -> m ())  -- action to release/clean-up the resource
          -> (a -> m b)   -- do sth with the resource
          -> Resource m b
```

## 1.1 The `Tactics` DSL

the DSL has 3 primitives:
1. `runT`, the first computation to be run in the environment
2. `bindT`, subsequent computations _in the same environment_ should use `bindT`
3. `pureT`, any first-order constructors which appear in a higher-order context must use `pureT` to satisfy the type-checker.

# 2 The Scope Effect 

> For some kinds of effects, it can be hard to design an expressive interface due to the semantics of their primitive resources. One instance of those are **resources whose lifetime is scoped to a small part of a program** (called a _region_ in this post), like a database connection.

type signatures

```haskell

data Scoped (param :: Type) (effect :: Effect)

interpretScoped 
  :: forall resource param effect r
  . ( forall q x 
      . param 
      -> (resource -> Sem (Opaque q ': r) x) 
      -> Sem (Opaque q ': r) x
    ) 
  -- ^ providing a resource using a bracket like signature
  -> (forall m x. resource -> effect m x -> Sem r x) 
  -- ^ execute effect using the resource
  -> InterpreterFor (Scoped param effect) r
```

- the `Opaque` type helps GHC for type inferencing, nothing more; `Sem (Opaque q: r)` is semantically equivalent to `Sem (q : r)`.
- the 1st argument is a similar to a `bracket` that is used to provide and cleanup a resource
- the 2nd argument specifies how to evaluate the effect

a special version of `interpretScoped`
```haskell
interpretScopedAs 
  :: forall resource param effect r
  . (param -> Sem r resource) 
  -> (forall m x. resource -> effect m x -> Sem r x) 
  -> InterpreterFor (Scoped param effect) r
```

a more general version of `interpretScoped`

# 3 References

1. [Polysemy is fun!](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/index.html)
2. [Scoped effect resources for polysemy](https://www.tweag.io/blog/2022-01-05-polysemy-scoped/)
3. [Introduction to free monads](https://serokell.io/blog/introduction-to-free-monads)
4. [Hackage: free](https://hackage.haskell.org/package/free)
5. [Hackage: polysemy](https://hackage.haskell.org/package/polysemy)
6. Kiselyov, O., & Ishii, H. (2015). Freer monads, more extensible effects. _SIGPLAN Not._, _50_(12), 94–105. [https://doi.org/10.1145/2887747.2804319](https://doi.org/10.1145/2887747.2804319)

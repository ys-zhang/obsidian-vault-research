#Haskell 

[The Haskell Wiki Book](https://en.wikibooks.org/wiki/Haskell)

# Toolchain

## The management tool "stack" 

use [stack](https://docs.haskellstack.org/en/stable/README/) instead of `cabal`

- `stack update`
- `stack setup`: install `ghc`
- `stack ghci`, `stack ghc`, `stack runghc`, `stack exec`: new repl
- `stack install <package>`



# Types

## Kind

<iframe width="560" height="315" src="https://www.youtube.com/embed/JleVecHAad4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

<iframe width="560" height="315" src="https://www.youtube.com/embed/Qy_yxVkO8no" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Implicit `forall` quantifier

for any unquantified type variable, the compiler will automatically insert a `forall`/$\forall$ quantifier.

```haskell
length :: [a] -> Int
length :: forall a . [a] -> Int
fst :: (a, b) -> a
fst :: forall a . forall b . (a, b) -> a
```

## Higher Kind Types

>[!tldr] 
> 1. a rank-0 type is a type without quantified types
> 2. a rank -1 type is a type only have the most outside quantifier
> 3. a **rank-n type**is a function that has at least one rank-(n-1) argument but no arguments of any higher rank.

The following 2 functions have different types 
```haskell
foo :: (forall a . a -> a) -> (Char, Bool)  -- rank 2
bar :: forall a . (a -> a) -> (Char, Bool)  -- rank 1
```

The `forall` at the _outermost_ level means that `bar` promises to work with any argument `f` as long as `f` has the shape `a -> a`for _some_ type `a` unknown to `bar`. 
Contrast this with `foo`, where it's the argument `f` which promises to be of shape `a -> a` for all types `a` **at the same time** , and it's `foo` which makes use of that promise by choosing both `a = Char` and `a = Bool`.

# Evaluation

An _expression_ is either a _redex (reducible expression)_  or in _normal form_ i.e., irreducible. 

Reduction strategies:

1. __innermost reduction__, a redex is an _innermost redex_ if it has no other redex as subexpressions inside it.
2. **outermost reduction** and always reduces **outermost redex**es that are not inside another redex.
# Library

## Type classes

#### Bifunctor

```haskell
 class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    -- | Map over both arguments at the same time.
    --
    -- @'bimap' f g ≡ 'first' f '.' 'second' g@
    --
    -- ==== __Examples__
    -- >>> bimap toUpper (+1) ('j', 3)
    -- ('J',4)
    --
    -- >>> bimap toUpper (+1) (Left 'j')
    -- Left 'J'
    --
    -- >>> bimap toUpper (+1) (Right 3)
    -- Right 4
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g


    -- | Map covariantly over the first argument.
    --
    -- @'first' f ≡ 'bimap' f 'id'@
    --
    -- ==== __Examples__
    -- >>> first toUpper ('j', 3)
    -- ('J',3)
    --
    -- >>> first toUpper (Left 'j')
    -- Left 'J'
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id


    -- | Map covariantly over the second argument.
    --
    -- @'second' ≡ 'bimap' 'id'@
    --
    -- ==== __Examples__
    -- >>> second (+1) ('j', 3)
    -- ('j',4)
    --
    -- >>> second (+1) (Right 3)
    -- Right 4
    second :: (b -> c) -> p a b -> p a c
    second = bimap id
```

# Template Haskell

```haskell
{- LANGUAGE TemplateHaskell -}
import Language.Haskell.TH
```

AST groups:
1. expression (`infixE`, `[e|1+1]` or `[|1+1|]`)    
2. pattern
3. declaration
4. type

# Cabal 

- generate dependency constrains: `cabal gen-bounds`, `cabal outdated`
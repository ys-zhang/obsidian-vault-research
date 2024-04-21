#Haskell #functional-programming #category-theory 

>[!Tldr] 
>Just like monads are values with effects, arrows are just functions with effects when applied. 

# Arrow Theory

## Homomorphisms in Category

recall morphisms in a category
```haskell
class Category (a :: k -> k -> Type) where
  id :: a x x
  (.) :: a y z -> a x y -> a x z

(>>>) :: Category a => a x y -> a y z -> a x z
f >>> g = g . f

(<<<) :: Category a => a y z -> a x y -> a x z
g <<< f = g . f
```

## Deal with product types

the arrow class is just a morphism that can deals better with _product_ of types

```haskell
-- | add support for product types
class Category a => Arrow a where
  -- | turn a pure function to an effectful arrow
  arr :: (x -> y) -> a x y
  -- deals with product types
  first :: a x y -> a (x, t) (y, t)
  second :: a x y -> a (s, x) (s, y)
  (***) :: a x y -> a s t -> a (x, s) (y, t)
  -- | fan-out
  (&&&) :: a x y -> a x z -> a x (y, z)

(^>>) :: Arrow a => (x -> y) -> (a y z) -> a x z
f ^>> g = (arr f) >>> g

(>>^) :: Arrow a => (a x y) -> (y -> z) -> a x z
f >>^ g = f >>> (arr g)

(^<<) :: Arrow a => (y -> z) -> (a x y) -> a x z
f ^<< g = (arr f) <<< g

(<<^) :: Arrow a => (a y z) -> (x -> y) -> a x z
f <<^ g = f <<< (arr g)

```

## Flow control and sum types

when we may also need support _sum_ of types, which leads to `ArrowChoice`:

```haskell
-- | add support for sum types
class Arrow a => ArrowChoice a where
  -- | like `mapLeft` in the `extra` package
  left :: a x y -> a (Either x t) (Either y t)
  -- | like `mapRight` in the `extra` package
  right :: a x y -> a (Either s x) (Either s y)
  (+++) :: a x y -> a s t -> a (Either x s) (Either y t)
  -- | fan-in: like `either`
  (|||) :: a x z -> a y z -> a (Either x y) z

ifA :: ArrowChoice arr => arr x Bool -> arr x y -> arr x y -> arr x y
filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
```

## Feedback loop

to connect output and input in a feedback style we need `ArrowLoop`
```haskell
class Arrow arr => ArrowLoop arr where
  loop :: arr (x, s) (y, s) -> arr x y

instance ArrowLoop (->) where
  loop :: ((x, s) -> (y, s)) -> x -> y
  loop f = \x -> let (y, s) = f (x, s)
                 in  y

```

the intention of this function is to construct a new arrow by feed the output `s` back to the input `s` of the original function 

>[!note] ArrowLoop trick
> Consider the function `g` where `g = loop f`.
>
> The semantics of value `g x` is to **repetitively** call the function `f (x, s)` with `x` fixed _up to the case when `s` is not demanded in the function body_.
>
> Thus, to make $loop(f) \neq \bot$ we need `f` to be recursive and have a base case matching on the pattern `(x, _)`, in other words,
> ``` haskell 
>  f :: (x, s) -> (y, s)
>  f (pat_x_base, _) = ...               -- base case
>  f (pat_x_rec, s)  = ... f (x', _) ... -- recursive case
> ```
> The IDEA is 
> - in the _base case_ (relative to the feed back argument) we do not demand the feedback argument to avoid endless loop, but we need to generate a new `s` and recursively call `f`
> - in the _recursive case_, we must have some branch which _do not recursively call `f`_
> In other words, 
> - the _base case_ we 
>   - do not (recursively) demand the feedback argument
>   - but recursively call `f` itself
> - the _recursive case_ we 
>   - do demand the feedback argument
>   - but _do not always_ recursively call `f` itself

There is an example `doWhileA` using `loop` in [[#Arrow Practices]]; but before that, let us first see an example using `loop` for pure arrows:
```haskell
doWhile :: forall a b . (a -> (b, Maybe a)) -> (a -> b)
doWhile f = arr Left >>> loop g 
 where 
  -- Left means we are in a non recursive call
  g :: (Either a a, Maybe a) -> (b, Maybe a)
  -- base case 
  g (Left x, _) = case f x of 
                     (y, Nothing) -> (y, Nothing)
                     (y, Just x') -> g (Right undefined, Just x')
  -- recursive case
  g (_, Just x)  = case f x of 
                    (y, Nothing) -> (y, Nothing)
                    (y, Just x') -> g (Right undefined, Just x')
```

## High order arrow

>[!tldr]
> Can we construct arrows which receive other arrows in their input, and invoke them?

Recall high order function is a function that can take function parameters.

```haskell
apply :: (a -> b, a) -> b

class Arrow arr => ArrowApply arr where
  app :: arr (arr x y, x) y

leftApp :: ArrowApply a => a x y -> a (Either x s) (Either y s)
```

### ArrowApply implies Arrow and ArrowChoice

You can use `app` and `Category` to define `first` and `left`

```haskell
firstByApp
  :: forall arr x y s
   . Arrow arr
  => (forall p q. arr (arr p q, p) q)
  -- ^ the app function
  -> arr x y
  -> arr (x, s) (y, s)
firstByApp app f = f' >>> app
 where 
  f' :: arr (x, s) (arr x (y, s), x)
  f' = arr $ \(x, s) -> let g = f >>> k  :: arr x (y, s)
                            k = arr $ \y -> (y, s)
                        in  (g, x)

leftByApp
  :: forall arr x y s
   . Arrow arr
  => (forall p q. arr (arr p q, p) q)
  -- ^ the app function
  -> arr x y
  -> arr (Either x s) (Either y s)
leftByApp app f = f' >>> app
 where
  f' :: arr (Either x s) (arr x (Either y s), x)
  f' = arr $ \case Left x  -> (f >>> arr Left, x)
                   Right s -> (arr $ const (Right s), undefined)  
```

### ArrowApply is equivalent to Monad

```haskell
newtype ArrowMonad arr a = ArrowMonad (arr () a)

instance ArrowApply arr => Monad (ArrowMonad arr) where
  return x = ArrowMonad $ arr (const x)
  (>>=) :: forall x y 
        .  ArrowMonad arr x 
        -> (x -> ArrowMonad arr y) 
        -> ArrowMonad arr y
  (ArrowMonad mx) >>= f = ArrowMonad $ (g &&& arr id) >>> app
   where 
    g :: arr () (arr () y)
    g = mx >>> (arr f) >>> unwrapArrowMonad 
    unwrapArrowMonad = arr $ \(ArrowMonad m) -> m
```

# Arrow Practices

## Pointless version

```haskell
combineA :: Arrow arr => (a -> b -> c) -> arr x a -> arr x b -> arr x c
combineA f arrA arrB = (arrA &&& arrB) >>> (arr $ uncurry f) 
```

We need `ArrowChoice` to define _if_ and _recursion_ on generalised arrows:

```haskell
ifA :: ArrowChoice arr 
    => arr x Bool   -- ^ test
    -> arr x y      -- ^ true  branch
    -> arr x y      -- ^ false branch
    -> arr x y
ifA arrBool arrTrue arrFalse = 
      arrBool &&& (arr id) 
  >>> (arr $ \(b, x) -> if b then Right x else Left x)
  >>> (arrFalse ||| arrTrue)

mapA :: forall arr x y . ArrowChoice arr => arr x y -> arr [x] [y]
mapA f = caseA >>> 
  (    arr (const [])                            -- base case
   ||| ( f *** (mapA f) >>> arr (uncurry (:)) )  -- recursive case
   )
 where
  caseA :: arr [x] (Either () (x, [x])) 
  caseA = arr $ \case []   -> Left ()
                      x:xs -> Right (x, xs) 

filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA f = mapA g >>> arr (map snd . filter fst)
 where
  g = f &&& arr id

-- | doWhileA generalises the `unfold` function in `Data.List.NonEmpty`
doWhileA :: forall arr a b 
         .  (ArrowLoop arr, ArrowChoice arr)
         => (arr a (b, Maybe a)) 
         -> (arr a b)
doWhileA f = arr Left >>> loop g
 where
  g :: arr (Either a a, Maybe a) (b, Maybe a)
  g = ifA (arr fst >>> arr isLeft)
          -- base branch
          (arr fst >>> unwrapEither >>> f >>> g')  
          -- recursive branch
          (arr snd >>> arr fromJust >>> f >>> g')
  g' :: arr (b, Maybe a) (b, Maybe a)
  g' = ifA (arr snd >>> arr isJust)
           ((first $ arr $ const (Right undefined)) >>> g)
           (arr id)
  unwrapEither = arr (either id id)
  isLeft (Left _) = True
  isLeft _ = False
  isJust (Just _) = True
  isJust _ = False

```


## Pointed version

```haskell
{-# LANGUAGE Arrow #-}

combineA' :: Arrow arr => (a -> b -> c) -> arr x a -> arr x b -> arr x c
combineA' f p q =
  proc x -> do
    a <- p -< x
    b <- q -< x
    returnA -< f a b

ifA' :: ArrowChoice arr => arr x Bool -> arr x y -> arr x y -> arr x y
ifA' check true false =
  proc x -> do
    p <- check -< x
    if p
      then true -< x
      else false -< x
      
mapA' :: forall arr x y. ArrowChoice arr => arr x y -> arr [x] [y]
mapA' f = proc xs -> case xs of 
  [] -> returnA -< []
  x:xs' -> do 
    y <- f -< x
    ys <- mapA' f -< xs'
    returnA -< y:ys

filterA' :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA' p = proc xs -> case xs of
  [] -> returnA -< []
  x : xs' -> do
    ok <- p -< x
    ys <- filterA' p -< xs'
    if ok
      then returnA -< ys
      else returnA -< x : ys
```

# The _Arrow notation_ language extension

## The `proc` arrow abstraction syntax

1. `proc pat -> command` where 
   - `proc` is a keyword
   - `pat`  is a pattern
   - `body` is a series of _commands_
2. The _dot_ command `-<`: 
    - `proc pat -> arrow -< expr` ==> `(arr $ \pat -> e) >>> arrow`
3. (deprecate) the _app_ command `-<<`:
    - `proc (f, x) -> f -<< x`  ==> `proc (f, x) -> app -< (f, x)`
    - the _app_ command is deprecated since `ArrowApply` is equivalent to monad, its better to use _monadic syntax_
4. The _return_ command `returnA = arr id` 
4. the _if_ rule:
    ```haskell 
         proc pat -> if expr then command1 else command2
    -- translates to ==>      
         arr (\pat -> if expr then Left pat 
                              else Right pat)
         >>> (proc pat -> command1) ||| (proc pat command2)
    ```
5. the _case_ rule extends _if_ rule
    ```haskell 
         proc pat -> case expr of
           pat1 -> command1 
           pat2 -> command2
           pat3 -> command3
           ...
           patN -> commandN 
    -- translates to ==>      
         arr (\pat -> case expr of 
                 pat1 -> Left pat1
                 pat2 -> Right . Left $ pat2
                 pat3 -> Right . Right . Left $ pat3
                 ...
                 patN -> Right . Right . Right ... . Right $ patN
         >>>      (proc pat1 -> command1) 
              ||| (proc pat2 -> command2)
              ||| (proc pat3 -> command3)
              ...
              ||| (proc patN -> commandN)
              ...
    ```
   
## The arrow `do` syntax

The `do` syntax allows binding names to intermediate values within a arrow `proc`'s body.
```haskell
proc pat -> do 
  x <- command1
  command2
-- transforms to ==>
    (arr id &&& proc pat -> command1)
>>> proc (pat, x) -> command2
```

## pointed feedback loop

we have the `rec` keyword to deal with feedback loop, _a group of bindings can be preceded by `rec` to make them recursive using loop._

```haskell
flipflop :: SF (Bool,Bool) (Bool,Bool) 
flipflop = proc (reset,set) -> do 
  rec c <- delay False -< nor reset d 
      d <- delay True -< nor set c 
  returnA -< (c,d) 
 where 
  nor a b = not (a || b)
```

## arrow combinator 

```haskell
proc pat -> (| e command1 ... commandN |) 
-- transformes to ==>
e (proc pat -> command1) ... (proc pat -> commandN)
```

# Full Code 

```haskell
{-#LANGUAGE LambdaCase#-}
module Main where

import Control.Arrow
import Control.Category qualified as Cat
import Data.Maybe (fromJust)
-- ====================================================================
-- # Arrow combinators
-- ====================================================================

combineA :: Arrow arr => (a -> b -> c) -> arr x a -> arr x b -> arr x c
combineA f arrA arrB = (arrA &&& arrB) >>> (arr $ uncurry f) 

ifA :: ArrowChoice arr 
    => arr x Bool   -- ^ test
    -> arr x y      -- ^ true  branch
    -> arr x y      -- ^ false branch
    -> arr x y
ifA arrBool arrTrue arrFalse = 
      arrBool &&& (arr id) 
  >>> (arr $ \(b, x) -> if b then Right x else Left x)
  >>> (arrFalse ||| arrTrue)

mapA :: forall arr x y . ArrowChoice arr => arr x y -> arr [x] [y]
mapA f = caseA >>> 
  (    arr (const [])                            -- base case
   ||| ( f *** (mapA f) >>> arr (uncurry (:)) )  -- recursive case
   )
 where
  caseA :: arr [x] (Either () (x, [x])) 
  caseA = arr $ \case []   -> Left ()
                      x:xs -> Right (x, xs) 

filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA f = mapA g >>> arr (map snd . filter fst)
 where
  g = f &&& arr id
  
doWhile :: forall a b . (a -> (b, Maybe a)) -> (a -> b)
doWhile f = arr Left >>> loop g 
 where 
  -- Left means we are in a non recursive call
  g :: (Either a a, Maybe a) -> (b, Maybe a)
  -- base case 
  g (Left x, _) = case f x of 
                     (y, Nothing) -> (y, Nothing)
                     (y, Just x') -> g (Right undefined, Just x')
  -- recursive case
  g (_, Just x)  = case f x of 
                    (y, Nothing) -> (y, Nothing)
                    (y, Just x') -> g (Right undefined, Just x')
  
doWhileA :: forall arr a b 
         .  (ArrowLoop arr, ArrowChoice arr)
         => (arr a (b, Maybe a)) 
         -> (arr a b)
doWhileA f = arr Left >>> loop g
 where
  g :: arr (Either a a, Maybe a) (b, Maybe a)
  g = ifA (arr fst >>> arr isLeft)
          -- base branch
          (arr fst >>> unwrapEither >>> f >>> g')  
          -- recursive branch
          (arr snd >>> arr fromJust >>> f >>> g')
  g' :: arr (b, Maybe a) (b, Maybe a)
  g' = ifA (arr snd >>> arr isJust)
           ((first $ arr $ const (Right undefined)) >>> g)
           (arr id)
  unwrapEither = arr (either id id)
  isLeft (Left _) = True
  isLeft _ = False
  isJust (Just _) = True
  isJust _ = False

-- ====================================================================
-- # Stream Function
-- ====================================================================

newtype SF a b = SF { runSF :: [a] -> [b] } 

instance Cat.Category SF where
  id = SF (map id)
  f . g = SF ( (runSF f) . (runSF g) )

instance Arrow SF where
  arr = SF . map 
  first (SF f) = SF $ \pairs -> f (fst <$> pairs) `zip` (snd <$> pairs)

instance ArrowChoice SF where
  left (SF f) = SF $ \xs -> 
    let ys = f [y | Left y <- xs]
        comb [] _ = []
        comb ((Left _): xs') (y':ys') = (Left y'): comb xs' ys'
        comb ((Right x'): xs') ys' = (Right x'): comb xs' ys'
    in  comb xs ys

instance ArrowLoop SF where
  loop :: SF (x, s) (y, s) -> SF x y
  loop (SF f) = SF $ \as -> 
      let (bs,cs) = unzip (f (zip as (stream cs))) 
      in  bs 
   where
    stream ~(x:xs) = x:stream xs

delay :: a -> SF a a 
delay x = SF (init . (x:))

main :: IO ()
main = do 
  print $ runSF (delay 0) [1, 4, 6]
  print $ runSF (mapA (delay 0)) [[1,2,3],[4,5,6],[7,8,9]]
  let f i | i <= 0    = do print "Cool" 
                           return ((), Nothing)
          | otherwise = do print $ "Keep going " ++ show i
                           return ((), Just (i - 1))
      g = doWhileA (Kleisli f)
  runKleisli g 10
```

# See also 

- [[FRP (Functional Reactive Programming)]]

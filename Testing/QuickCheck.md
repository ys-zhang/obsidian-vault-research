#property-based-testing


# Generate

```haskell
-- | generate with size = 30 
generate :: Gen a -> IO a

newtype Gen a = MkGen 
  { unGen :: QCGen  -- random generator
          -> Int    -- size
          -> a
  } 
  deriving (Functor, Applicative, Monad, MonadFix)

data QCGen = MkStdQCGen StdGen | MkSMQCGen MSGen
  
class Arbitrary a where
  arbitrary :: Gen a
```

>[!note] "size" 
> The `size` argument of `Gen a` has no default meaning. The object of `Gen a` can give it different interpretation base on different implementation. It can be the size of an unbounded data type such as List or Tree, or the upper bound of the generated Integer.
>
> _Generally, from the perspective of random variable, `Gen a` represents a random variable with an integer parameterised distribution and the "size" is the parameter of that underlining probability distribution._


>[!note]
> The `Test.QuickCheck.Modifiers` module includes helper newtypes like `NonNegative`, `NonZero`, `Positive`, `OrderdedList` etc. Internally there is not much to these types, they simply allow a way for QuickCheck to define new Arbitrary instances.

## Generator Combinator

```haskell
-- | choose between the lower and upper bound
choose :: Random a => (a, a) -> Gen a
-- | one of the elements
elements :: [a] -> Gen a

oneOf :: [Gen a] -> Gen a

fequency :: [(Int, Gen a)] -> Gen a
```

## Common Generators

1. List 
```haskell
subliftOf :: [a] -> Gen [a]
shuffle :: [a] -> Gen [a]
orderedList :: (Ord a, Arbitrary a) => Gen [a] 
listOf :: Gen a -> Gen [a]
```

2. Number
```haskell
arbitrarySizedIntegral :: Integral a => Gen a

```

## Unbounded Data types

> [!note]
> In general when generating unbounded types like Integer or list the question naturally arises: how large an integer or long a list will we choose? 
>
> It’s impossible to pick from among all integers with equal probability. 
>
> QuickCheck resolves this conundrum by including the _size_ parameter in a generator.

recall the type `Gen a` basically its the function type
```haskell
type Gen a = StdGen -> Int -> a
```
while the `Int` argument can and usually serves as a suggestion of the size of the result unbound data type.

```haskell
-- | use different generator base on different size
sized :: (Int -> Gen a) -> Gen a
sized f = MkGen $ \r n -> 
  let g = f n 
  in g r s 

resize :: Int -> Gen a -> Gen a
resize n (MkGen g) = MkGen $ \r _ -> g r n

scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized $ \n -> resize (f n) g
```

generating lists with expected size
``` haskell
flexList :: Arbitrary a => Gen [a]
flexList :: sized $ \n -> 
  frequence 
    [ (1, return [])
    , (n, (:) <$> arbitrary <*> flexList)
    ]
```

$$
\begin{align}
E_n &= \frac{1 \times 0}{1+n} + \frac{n(1 + E_n)}{1+n} \\
\implies E_n &= n 
\end{align}
$$


## Monad Transformer 

Sometimes you need to generate monadic values, for instance testing some external system, such as web services. 

It may cause pain when generating these actions needs external information when often means you cannot create a pure generator such, instance the creation of the generator must be wrapped in some IO monad, for instance `mkGen :: IO (Gen (IO a))`, its even more painful if you need to chain generators, especially you have a [[generative model]].

To solve the problem we have the monad transformer `GenT`

```haskell
class MonadGen m where
  liftGen :: Gen a -> m a
  ...
  
newtype GenT m a = GenT { unGenT :: QCGen -> Int -> m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans GenT
instance MonadIO m => MonadIO (GenT m)
instance Monad m => MonadGen (GenT m)

runGenT :: GenT m a -> Gen (m a)
```

thus the above problem can solved by smash `IO (Gen (IO a))` to `GenT IO a`.

# Property

```haskell
newtype Property = MkProperty 
  { unProperty :: Gen (Rose Result) 
  }

data Result 
  = Success {..}
  | GaveUp {..}
  | Failure {..}
  | NoExceptedFailure {..} -- shall fail but not

quickCheck :: Testable prop => prop -> IO ()

class Testable prop where
  property :: prop -> Property
  ...

instance Testable prop  => Testable (Gen prop) where
  ...
instance (Testable prop, Arbitrary a) => Testable (a -> prop) where
  property f = property (f <$> arbitary)
```

>[!note] invariants property
> use the package [test-invariant](https://hackage.haskell.org/package/test-invariant) to create common invariants


>[!note] quickCheck
> the `quickCheck` function generates `Result`s starting from a small size parameter and gradually increase it. 


## Perspective from logic and probability


`Property` is actually `Gen Bool` per se, which is actually a boolean random variable. In other words it means something holds or not.

The type-class `Testable` is just the same concept of predicate since the `property :: a -> Property` is just a predicate over the type a.

From the perspective of truth value, `Gen Bool` itself is a truth value in [[Probabilistic Soft Logic & Hinge-loss Markov Random Field|Probabilistic Soft Logic]]. 

>[!def] predicate
> a _predicate_ on type/set `A` is a function
> $$ pred :: A \to TV $$
> where $TV$ is the type/set of _truth value_, which can be different in different types of logic.

If we treat `Gen Bool` i.e. `Property` as truth value in our new logic system, then the `property` function makes any `Testable` a truth value using push forward. Thus its easy to see anything maps to a Testable is a predicate.

## Combinators 

```haskell
-- | use custom generator when testing this property 
forAll :: Testable prop => Gen a -> (a -> prop) -> Property

-- | implies
(==>) :: Testable prop => Bool -> prop -> Property

-- | same as (==) but print counterexamples when fails
(===) :: Eq a => a -> a -> Property

-- | same as (/=) but print counterexamples when fails
(=/=) :: Eq a => a -> a -> Property

ioProperty :: Testable prop => IO prop -> Property
idempotentIOProperty :: Testable prop => IO prop -> Property
```

# Shrinking

>[!def] shrinking
> Shrinking is an operation defined per-type as an optional method in the Arbitrary class. 
>
> It has type `shrink :: a -> [a]`, returning a list of simpler potential counterexamples. Candidates are tried in the order they appear in the list.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
```

Most implementations of shrink should try at least three things:
1. Type-specific shrinking such as replacing a constructor by a simpler constructor.
2. Shrink a term to any of its immediate sub-terms. You can use `subterms` to do this.
3. _Recursively apply shrink to all immediate sub-terms._ You can use `recursivelyShrink` to do this.

>[!note] 
> In the first glance, it seems that the 3rd rule is not necessary; however it is critical to have the smallest counter-exampler.
>
> To see the reason, first notice how shrink function is used
> ```haskell
> runShrinker :: Arbitrary a => (a -> Bool) -> a -> a
> runShrinker p a 
>   | p a       =  error "not a counter-example"
>   | otherwise = 
>       case listToMaybe (filter (not . p) $ shrink a) of 
>         Nothing -> a 
>         Just a' -> runShrinker p a'
> ```
> to reach the minimum, you have to assure that there is at least one counter-example in the shrinking options, this may not be true if we only shrink sub-terms for 1 level.
> 
> For example, we if we shrink integer using the follow shrinker
> ```haskell
> shrink :: Integer -> [Integer]
> shrink a = [a-1]
> ```
> we probably will never reach the minimum



# Coverage

>[!FAQ] Assess/Improve confidence over test results
> 1. How to get the coverage of the samples have been tried?
> 2. How to force to coverage to some specific rate?


## Testing Statistics

```haskell
-- report distribution of `f(sample)`
-- `f` must be discrete random variable
collect :: (Show a, Testable prop)
        => a              -- the `f(sample)`
        -> prop           -- the test of the sample
        -> Property       

-- | report distribution of `f(sample)` 
-- while `f` is boolean random variable
classify :: Testable prop
         => Bool          -- if sample falls into the category
         -> String        -- label of the category
         -> prop          -- input  testable
         -> Property      -- output testable

-- | report distribution, while a test can generate multiple 
-- labels.
tabulate :: Testable prop
         => String          -- caption of the table
         -> [String]        -- tags of the sample
         -> prop            -- input  testable
         -> Property        -- output testable
```



# Functions

## Generating Functions

[How QuickCheck generates random functions](https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html)

we want some method that can automatically generate some function `f :: a -> b`, which means

```haskell
instance (? a, ? b) => instance Arbitrary (a -> b) where
  arbitrary :: Gen (a -> b)
```
where all functions of type `a -> b` can be generated from `arbitrary @(a -> b)`

**Observation**: `Gen (a -> b)` is equivalent to `a -> Gen b`

```haskell
  Gen (a -> b)
= StdGen -> Int -> (a -> b)
= StdGen -> Int -> a -> b
= a -> StdGen -> Int -> b 
= a -> Gen b
```

hence its very attempting to implement as:

```haskell
instance Arbitrary b => instance Arbitrary (a -> b) where
  arbitrary :: a -> Gen b
  arbitrary = const arbitrary
```

but this will be a problem as it can only generate constant functions, which are in the form of `const b :: a -> b`, in other words, `f(a0)` and `f(a1)` are strongly correlated. 

the `Gen b` in type `a -> Gen b` is actually the conditional expectation of $E_{(A, B)}[B|A]$, the above implementation makes the conditional expectation to be a point distribution, i.e., irrelevant of $A$.

>[!important]
> ***we need the result `Gen b` to be truely depend on `a`***.

the problem is how to make it automatically. the idea is to customise `arbitrary::Gen b` (which is uncorrelated with `a`) to a new `Gen b` base on `a`'s value, which leads to the definition of `coarbitrary`

```haskell
class Coarbitrary a where
  -- | use to customise `arbitrary :: Gen b`
  coarbitrary :: a -> Gen b -> Gen b
```

and the implementation:

```haskell
instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary :: Gen (a -> b)
  arbitrary = iso $ \a -> coarbitrary a (arbitrary @b)

iso :: (a -> Gen b) -> Gen (a -> b)
```

then the problem becomes how to implement `Coarbitrary`, the trick here is to temper the seed of the random generator, _QuickCheck_ provides the `variant` function:
```haskell
variant :: Integral n => n -> Gen a -> Gen a
```
thus we only need to implement a function of type `a -> Integer`

## Show and Shrinking Functions

<iframe width="560" height="315" src="https://www.youtube.com/embed/CH8UQJiv9Q4?si=A26RtlsuRkLO8C9f" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>


# Stateful Tests

## Methodology

in order to test the stateful system, we need a way to speak about its correctness, there are two methods:

1. use operation semantics to describe actions and how state transits 
2. use a model of the state and test against that model, we prove the correctness of the system by proving the correctness of the model

### Operational Semantics

>[!note]  
> The property of a state machine in operational semantics can with stated as 
> 1. (_precondition_) Given a property of the pre-state
> 2. (_generator_) and a property of the action
> 3. (_postcondition_) then the post-state must satisfies some property

Need a way to parameterise state, then the function which generates actions base on some initial state can be modelled as a function from the parameter of the state to an action generator.

>[!quote]
The key idea for coping with the monadic nature of the implementation was to define a "queue program language", represented as a Haskell datatype, and quantify over contexts. 

1. (parameterise action) model actions as some ADT. 
2. give a semantics of this language of actions that is 
```haskell
semantics :: Action            -- the action 
          -> Observation       -- the current collection of obvs
          -> System            -- system before the action
          -> (System, Obersivation) -- system and obvs aft action
```

### Model

Given a system $(S, A)$ where $S$ is the collection of states and $A$ is the collection of actions, beside transit states they can have return values. 

Also we have a model of the system which contains model of the states $S_M$ and model of the actions $A_M$. these actions can have type
```haskell
type Action = S -> (R, S)
```
where $R$ is some return(observable) value of the action.

>[!note] idea
> The idea is to test the model instead of the system


To do so we need to test 2 things
1. The model (or representation) is _correct_ if the _abstract arrow_ is a functor, for detail see [[General Theory of Representation and Abstraction]]: 
2. The property holds on the model.

## Test.QuickCheck.Monadic




## quickcheck-state-machine

> specify the correctness by means of a state machine based model using pre- and post-conditions.

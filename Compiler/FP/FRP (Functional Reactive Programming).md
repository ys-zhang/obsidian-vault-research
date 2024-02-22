#functional-programming  #reactive-programming  #Haskell 


# Behaviour and Event

1. _behaviour_ is a continuous time indexed value 
2. _event_ is a list of timestamped values 

```haskell
class Behaviour b a where
  at :: b a -> t -> t -> a

class Event e a where 
  occ :: e a -> t -> t -> [(t, a)]
```

Intuitively, the meaning of a **behaviour**, as given by `at` is a function mapping a _start time_ and a _time of interest_ to the value of the behaviour at the given time of interest.

The meaning of an **event**, given by `occ`, is a function that takes also a start time $T$ and a time of interest $t$, and returns a nite list of time-ascending occurrences of the event in the interval $(T, t]$

## `reflex` package

- [reflex-frp](https://reflex-frp.org)
- [An introduction to reflex](https://qfpl.io/posts/reflex/basics/introduction/)

```haskell
{-| The parameter t is for keeping track of 
    the "timeline" the Event happens on. 
    Each 
  -}
data Event t a     -- sth. like [(t, a)]

data Behavior t a  -- sth. like t -> a

data Dynamic t a   -- sth. like (Behavior t a, Event t a)

-- --------------------------------------------
-- construct a behavior
-- --------------------------------------------


-- | create a behavior/signal using an event's 
--   latest value
hold :: MonadHold t m
     -- ^ the monad represents a
     --   modification of the FRP network 
     --   in future frames.
     => a
     -- ^ initial value
     -> Event t a
     -- ^ the event to follow
     -> m (Behavior t a)

-- --------------------------------------------
-- read from a behavior 
-- --------------------------------------------

tag :: Reflex t
    => Behavior t a 
    -- ^ determine the value of the result event
    -> Event t b 
    -- ^ determine when the result event happens
    -> Event t a

-- | read value of a behavior at the current
--   time (at which we are still in construction
--         of the network)
sample :: MonadSample t m 
       => Behavior t a 
       -> m a

-- --------------------------------------------
-- Dynamic as a prod of Event & Behavior
-- --------------------------------------------
updated :: Reflex t => Dynamic t a -> Event t a
current :: Reflex t => Dynamic t a -> Behavior t a

holdDyn :: (Reflex t, MonadHold m)
        => a 
        -> Event t a
        -> m (Dynamic t a)

-- | fold through a series of event firings
foldDyn :: (Reflex t, MonadHold m, MonadFix m)
        => (a -> b -> b) 
        -> b
        -> Event t a
        -> m (Dynamic t b)
```


>[!NOTE] FRP frame or transaction
> `reflex` uses a discrete semantics model, time line is separated into _frames_:
> 1. values behaviours are only allowed to change at the endpoints of each frame, i.e., during each frame all behaviour has fixed value.
> 2. Multiple events can happen in a single frame, but _no more than one_ of them is triggered by the outside world.
> 
> The benefit of this approach is to free us from wrangling with the order of events within a frame, since behaviours/signals are constant within a frame, event order has no effect on the FRP network state. 


# AFRP (Arrowized FRP)

## Basic Concepts

```haskell
-- | abbr. signal function, i.e., a function creates a signal from a signal
newtype SF a b = SF { runSF :: Signal a -> Signal b }

-- | also dubbed as Bahavior
newtype Signal a = Signal { at :: Time -> a }
```

```haskell
data Event a = Event a | NoEvent

-- | Binds a given value to an event occurrence
tag :: Event a -> b -> Event b
tag e b = fmap (const b) e 

lMerge :: Event a -> Event a -> Event a 
lMerge left@(Event _) _ = left
lMerge NoEvent right    = right
mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a 
mergeBy f = liftA2 f
```

```haskell
-- | generates an event when a boolean signal
--   alts from false to true
edge :: SF Bool (Event ())

-- | the result SF behaves as 
--   1. same as the first result signal of the first param
--   2. once Event c first happens, switch to whatever 
--      the second param returns
--   3. no other switches of following Event c
switch :: SF a (b, Event c)  
       -> (c -> SF a b) 
       -> SF a b

switch' :: SF a b 
        => SF a (Event c)
        -> (c -> SF a b)
        -> SF a b

-- | recurring switch, same as `switch` except the output
--   keep changing whenever Event c happens
rSwitch :: SF a (b, Event c)  
        -> (c -> SF a b) 
        -> SF a b

par :: (Functor col) -- a collection
    => (forall sf . a -> col sf -> col (b, sf))
    -- ^ bind each signal function in a collection with a 
    --   an input value of type b
    -> col (SF b c)   -- a collection of signal functions
    -> SF a (col c)

-- | suffix B stands for broadcast
parB :: (Functor col)
     => col (SF a b)
     -> SF a (col b)

pSwitch :: Functor col
        => (forall sf . a -> col sf -> col (b, sf))
        -> col (SF b c)
        -> SF (a, col c) (Event d)
        -> (col (SF b c) -> d -> SF a (col c))
        -> SF a (col c) 
        
```


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


## A Stream Implementation

```haskell
type Bahavior a = [Time] -> [a]
type Event a = [Time] -> [Maybe a]

at'' :: Bahavior a -> [Time] -> a
at'' b ts = last $ b ts 

occ'' :: Event a -> [Time] -> [(Time, a)]
occ'' e ts = [ (t, fromJust mbA) | (t, mbA) <- zip ts $ e ts
                                , isJust mbA ]

at'  :: Behavior a -> Time -> Time -> a
occ' :: Event a -> Time -> Time -> [(Time, a)]
```

We want `at'` and `occ'` to be 
1. the limit of `at''` and `occ''` when the gap of the partition goes to $0$,
2. or $\bot$ if the limit does not exist

> It can be proven that `at''` converge to `at'` uniformly on $\mathbb R$.

1. lift
```haskell
<*> :: Behavior (a -> b) -> Behavior a -> Behavior b
bf <*> ba = \ts -> zipWith ($) (bf ts) (ba ts)

lift0 :: a -> Behavior a
lift0 x = map (const x)

lift1 :: (a -> b) -> Behavior a -> Behavior b 
lift1 f b1 = lift0 f <*> b1 

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c 
lift2 f b1 b2 = lift1 f b1 <*> b2
```
2. 


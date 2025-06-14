# 1 Design Space

1. how to encode time?
2. how to encode probability
3. how to make it composable


> cashflow is derived from contracts and observations

# 2 Case study


## 2.1 Composable Contract

|  Concept   |  Naming   |
| :--------: | :-------: |
|  Contract  | `c, d, u` |
| Observable |    `o`    |
|    Time    |  `t, s`   |
|  Currency  |    `k`    |

The following concepts are related to contract **valuation**:

- (_acquisition date_) the date that the contract takes effect
- (_horizon/expire date_) the last date that the contract can be acquired

>[!note]
> _horizon_  is a property of a contract while _acquisition date_ is not. 
>
> To be more specific, horizon is part of contract definition while acquisition is associated with a contract instance.

### 2.1.1 Primitive contracts

1. _zero-coupon discount bound_
```haskell
zcb :: Date -> Double -> Currency -> Contract

zcb' :: TimeLine t => t -> Double -> Currency -> Contract' t
```


# 3 Blueprint

```haskell
-- | `t` for time line
-- `e` for input observable
-- `a` for output observable
data Contract t e a

  -- | an observable in time line `t` of value type `a`
data Obv t a

-- | a time line is usually bounded to assumptions
class TimeLine t where
  onDay :: Date -> t

instance TimeLine Date where
  onDay = id

```

A _contract_ is a functions that maps events to obligations/rights
```haskell
compile :: TimeLine t => Contract t e a -> Obs t e -> Obs t a
```

## 3.1 IR

in order to support Excel as the compile target, we need to representing the type 
```haskell
Obs t e -> Obs t a
```
in Excel spreadsheets.

The _formula_ is an intermediate representation that is more close to excel spreadsheet.

we use _models_ to model spreadsheets, 
```haskell
data Model input output = Model 
  { inputs      :: [Either VarDecl Model] 
  -- ^ inputs are identifiers of input observables
  , lib         :: [ScDef]
  -- ^ lib will not be compiled to Excel columns
  -- we use lib to define utility functions 
  -- and intermediate data
  , obs         :: [ScDef]
  -- ^ obs are output observables
  }

compile :: Module input output -> (Obs t input) -> (Obs t ouput) 

compileToExcell -> Module input output -> SpreadSheet
```

to we use OCaml module syntax to demonstrate using _model_ language to model a _termed life_ insurance:
1. a termed life contract specifies 
   - how long the term is 
   - premium of the contract
   - on what event happens shall person insured get paid

```ocaml

module type TimeLine = sig 
  type t 
  val measure : t -> int
  val bow: t
  val eow: t
end

module type Event(T: TimeLine) = sig 
  type t 
  val col : t array
end

module type Termed (
    T: TimeLine,        (* the time line *)
    E: Event,        (* the trigger event *)
    Term  : sig val ... end
) = sig

  type time_t = T.t
  type event_t = E(T).t
  
  val income : int 't obs
  val payment: int 't obs
end
```


```ocaml
module TermedLife(
  P : Model_Premium, 
  Aquired: Model_Event, 
  E: Model_Insured
) = struct 
  let triggered: bool array = E.trans == Alive -> Death
  let payment: double array = triggered * P.sa: 
end
```

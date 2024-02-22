#Rust #incremental-computing

> [!summary]
> [Salsa](https://github.com/salsa-rs/salsa) is a Rust framework for writing incremental, on-demand programs -- these are programs that want to adapt to changes in their inputs, continuously producing a new output that is up-to-date. 
>
> Salsa is based on the the _incremental recompilation_ techniques that we built for `rustc`, and many (but not all) of its users are building compilers or other similar tooling.

# Red-green Algorithm

#algorithm 




# Computation Model 

_Salsa_ represents an acyclic computation graph with 
- inputs and intermediate computation results as **nodes**;
- _tracked functions_ as graph **edges** 

```haskell
module Salsa where

import Control.Comonad
import Data.Coerce (coerce)
import Data.IntMap.Strict as Map
import Data.Maybe
import GHC.Exts (Any)
import GHC.Stack (HasCallStack)
import Type.Reflection
import Unsafe.Coerce
```

## Nodes 

this corresponding to the following rust macro
1. `salsa::input`
2. `salsa::tracked` applying to structs

```haskell
newtype SalsaId a = SalsaId Int

type SalsaDb = IntMap Any

readDb :: HasCallStack => SalsaDb -> SalsaId a -> a
readDb db sid = unsafeCoerce (db Map.! coerce sid)

mkSalsa :: SalsaDb -> a -> Salsa a
mkSalsa db a =
  let a' = unsafeCoerce @_ @Any a
      k = maybe 0 ((+ 1) . fst) (Map.lookupMax db)
  in  Salsa{salsaId = coerce k, salsaDb = Map.insert k a' db}

data Salsa a = Salsa
  { salsaId :: !(SalsaId a)
  , salsaDb :: !SalsaDb
  -- ^ we only guarantee salsaDb contains all
  -- dependencies of this value
  }
  
instance Show a => Show (Salsa a) where
  show sa = "(salsa " ++ (show . unSalsaId . salsaId $ sa) ++ "): " ++ show (extract sa)

instance Functor Salsa where
  fmap = liftW

instance Comonad Salsa where
  extract :: Salsa a -> a
  extract Salsa{..} = readDb salsaDb salsaId
  extend :: (Salsa a -> b) -> Salsa a -> Salsa b
  extend f sa =
    let b = f sa
    in  mkSalsa (salsaDb sa) b

```
## Edges

This corresponding to the `salsa::tracked` applying to function.

The purpose of _tracked function_ is to allow Salsa to 
1. track which input the function accesses
2. memoize the returned value

Stated in [The official Doc](https://salsa-rs.netlify.app/salsa2022/overview#tracked-functions) _tracked function_ must satisfy:
1. They must take a `&`-reference to the database as their first argument;
2. They must take a "Salsa struct" as the second argument;
3. They _can_ take additional arguments, but it's faster and better if they don't.


# References

1. [The Salsa Book](https://salsa-rs.netlify.app/)
2. [Salsa Github repo](https://github.com/salsa-rs/salsa)
3. [Salsa Algorithm Explained](https://medium.com/@eliah.lakhin/salsa-algorithm-explained-c5d6df1dd291)

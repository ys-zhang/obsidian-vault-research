```haskell

-- | 1. parser from string to expression  
-- | 2. from expression to table
parse :: String -> Either String Table 

type Table = Map String Value 
data Value = Integer   Integer 
           | Bool      Bool
           | Float     Double 
           | String    String
           | TimeOfDay TimeOfDay
           | ZonedTime ZonedTime
           | LocalTime LocalTime
           | Day       Day
           | Array     [Value]
           | Table     Table


class FromValue a where 
  fromValue     :: Value -> Matcher a
  listFromValue :: Value -> Matcher [a]

class FromKey a where 
  fromKey :: String -> Matcher a

runMatcher :: Matcher a -> Result MatchMessage a

data Matcher a  = ...
  deriving (Monad, MonadPlus, ...)

```

operations to construct matchers

```haskell
-- constructing ParseTable
reqKey :: FromValue a -> String -> ParseTable a
optKey :: FromValue a -> String -> ParseTable (Maybe a)

-- use ParseTable to construct matcher
runParseTable :: ParseTable a -> Table -> Matcher a
runParseTableFromValue :: ParseTable a -> Value -> Matcher a
```
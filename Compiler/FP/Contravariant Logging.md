#Haskell  #logging

```haskell
import Data.Coerce (coerce)

newtype LogAction m msg = LogAction 
  { unLogAction :: msg -> m ()
  }

logStringStdout :: MonadIO m -> LogAction m String
logStringStdout = LogAction ( liftIO . putStrLn )

infix 5 <&
(<&) :: LogAction m msg -> msg -> m ()
(<&) = coerce
```

The type parameter's order, i.e., `LogAction m msg` instead of `LogAction msg m` to designed deliberately to make the type a [[Category Theory#Contravariant functor|Contravariant Functor]]

```haskell
import Data.Functor.Contravariant

instance Contravariant (LogAction m) where 
  contramap :: msg' -> msg -> LogAction m msg -> LogAction m msg'
  contramap f = LogAction . (. f) . unLogAction 
```

Monadic logger 

```haskell
import Control.Monad.Reader
import Data.Kind

newtype LoggerT msg m a = LoggerT 
  { runLoggerT :: ReaderT (LogAction (LoggerT msg m) msg) m a
  } deriving (Functor, Applicative, Monad, MonadIO
             , MonadReader (LogAction (LoggerT msg m) msg) 
             )
```

the top-level interface 

```haskell
class HasLog env msg m where 
  getLogAction :: env -> LogAction m msg
  setLogAction :: LogAction m msg -> env -> env 

type WithLog :: Type -> Type -> (Type -> Type) -> Constraint
type WithLog env msg m = (MonadReader env m, HasLog env msg m) 
-- ^ 1. you can read env from m
--   2. env has a log action: msg -> m ()

logMsg :: forall msg env m
       .  WithLog env msg m 
       => msg -> m ()
logMsg msg = do 
  LogAction log <- asks getLogAction
  log msg

-- | a better runLoggerT
usingLoggerT :: Monad m 
             => LogAction m msg
             -> LoggerT msg m a 
             -> m a
```

# Composing log actions

## Monoid: sequence multiple actions to one

```haskell
instance Applicative m => Semigroup (LogAction m msg) where
  LogAction a1 <> LogAction a2 = 
    LogAction $ \msg -> a1 msg *> a2 msg

instance Applicative m => Monoid (LogAction m msg) where
  mempty = LogAction (const $ pure ())
```


## Contravariant Functor

```haskell
cmap = contramap

cfilter :: Applicative m 
        => (msg -> Bool) 
        -> LogAction m msg 
        -> LogAction m msg
        
cmapM :: Applicative m 
      => (b -> m a) 
      -> LogAction m a 
      -> LogAction m b
```

### Divisible & Decidable

split some complex data structure into smaller pieces and if you know how to log each piece independently, you can now log the whole data structure.

```haskell
-- analog of Applicative
class Contravariant f => Divisible f where
  conquer :: f a
  divide  :: (a -> (b, c)) -> f b -> f c -> f a
  -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c

-- analog of Alternative
class Divisible f => Decidable f where
  -- | act as identity to `choose` 
  lose :: (a -> Void) -> f a
  choose :: (a -> Either b c) -> f b -> f c -> f a

(>$<) :: Contravariant f => (b -> a) -> f a -> f b
(>*<) :: Divisible     f => f a -> f b -> f (a, b)
(>|<) :: Decidable     f => f a -> f b -> f (Either a b)
(>*)  :: Divisible     f => f a -> f () -> f a
(*<)  :: Divisible     f => f () -> f a -> f a
```

## Comonad 

```haskell
-- dual of monad
class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  -- extend' :: w a -> w (w a)

newtype Traced m a = Traced { runTraced :: m -> a }
instance Monoid m => Comonad (Traced m) where
  extract (Traced ma) = ma mempty 
  extend f (Traced ma) = Traced $ \m -> 
    f (Traced $ \m' -> ma (m <> m'))
```
Log action as comonad
```haskell
type LogAction' m msg = Traced msg (m ())

extract :: Monoid msg => LogAction m msg -> m ()
extract (LogAction action) = action mempty

extend
    :: Semigroup msg
    => (LogAction m msg -> m ())
    -> LogAction m msg
    -> LogAction m msg
extend f (LogAction action) =
    LogAction $ \m -> f $ LogAction $ \m' -> action (m <> m')
```

# Configuration: extensible record interface

```haskell

type family FieldType (fieldName :: Symbol) :: Type

newtype MessageField (m :: Type -> Type) (fieldName :: Symbol) 
  = MessageField
      { unMessageField :: m (FieldType fieldName)
      }

type FieldMap (m :: Type -> Type) 
  = TypeRepMap (MessageField m)

defaultFieldMap :: MonadIO m => FieldMap m
defaultFieldMap = 
  fromList
    [ WrapTypeable $ MessageField @_ @"threadId" (liftIO myThreadId)
    , WrapTypeable $ MessageField @_ @"utcTime"  (liftIO getCurrentTime)
    ]

-- use the OverloadedLabels LangExt
instance (KnownSymbol fieldName, a ~ m (FieldType fieldName))
      => IsLabel fieldName (a -> WrapTypeable (MessageField m)) where
  fromLabel field = 
    WrapTypeable $ MessageField @_ @fieldName field

defaultFieldMap :: MonadIO m => FieldMap m
defaultFieldMap = fromList
    [ #threadId (liftIO myThreadId)
    , #utcTime  (liftIO getCurrentTime)
    ]
```
# References
- [CO-LOG: COMPOSABLE CONTRAVARIANT COMBINATORIAL COMONADIC CONFIGURABLE CONVENIENT LOGGING](https://kowainik.github.io/posts/2018-09-25-co-log)

<iframe width="560" height="315" src="https://www.youtube.com/embed/qzOQOmmkKEM?si=GahPPYAMXWeDxNm-" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
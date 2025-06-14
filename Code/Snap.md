#Haskell  #webserver #Lens #Monad 

# Snap

>[!tldr]
> In general, you will use the `Handler` monad defined in `Snap.Snaplet`, which is an instance of `MonadSnap`

the `Snap` monad provides functionality of extending the HTTP server using _middleware_ by 
1. Stateful access to fetch or modify an HTTP Request.
2. Stateful access to fetch or modify an HTTP Response.
3. Failure handling through `Alternative` and `MonadPlus`

```haskell
data Snap a 
 deriving ( MonadSnap
          , Monad, MonadIO, MonadFail, MonadPlus
          , MonadBase, MonadBaseControl -- Exception handling
          , Applicative, Alternative
          , Functor)

class (Monad m, ...) => MonadSnap m where
  liftSnap :: Snap a -> m a
```

# Snaplet

A **snaplet** is a _composable_ web application.

> A snaplet can represent anything from backend Haskell infrastructure with no user facing functionality from a small widget like a chat box that goes in the corner of a web page to an entire standalone website like a blog or forum.

Snaplet API category:
1. (_app state_) Infrastructure for application state/environment
2. (_life cycle_) Snaplet initialisation, reload, and cleanup
4. (_install_) Management of filesystem data and automatic snaplet installation
3. (_config_) Unified config file infrastructure

>[!warning]
>**use `MVar` or `STM` if you needs global state shared between requests**
>
>The Snap web server processes each request in its own green thread. This means that each request will receive a separate copy of the state defined by your application and snaplets, and _modifications to that state only affect the local thread_ that generates a single response.

## State management

```haskell
data Snaplet s
```

An instance of `Snaplet s` is a product of
- a value of type `s`, called the "user state".
- some bookkeeping data the framework uses to plug things together:
  - the snaplet's configuration
  - the snaplet's root directory on the filesystem
  - the snaplet's root URL, and so on.

> [!note] `MonadSnaplet`
> The primary abstraction in the snaplet infrastructure is a combination of the reader and state monads. 
>   - The _state_ monad holds the top level application data type (from now on referred to as the **base state**). 
>   - The _reader_ monad holds _a lens from the base state to the current snaplet's state_.

```haskell
type SnapletLens s a = ALens' s (Snaplet a)

class MonadSnaplet (m :: * -> * -> * -> *) where
  -- Runs a child snaplet action in the current snaplet's context.  
  with :: SnapletLens v v'
       -- ^ lens from parent state to child state
       -> m b v' a 
       -- ^ child state action
       -> m b v  a
       -- ^ parent state action
  {-| Using our filesystem metaphor, the lens for 
      this function must be an absolute path, i.e. 
      it's base must be the same as the current base -}
  withTop :: SnapletLens b v'
          -> m b v' a
          -> m b v a
  ... 

instance MonadSnaplet Initializer
instance MonadSnaplet Handler
```

## Create Snaplet

```haskell
-- | this type just for forcing all snaplets are intialized through
-- `makeSnaplet` and `nestSnaplet`
newtype SnapletInit b v = SnapletInit (Initializer b v (Snaplet v))

newtype Initializer b v a =
    Initializer (LensT (Snaplet b)
                       (Snaplet v)
                       (InitializerState b)
                       (WriterT (Hook b) IO)
                       a)
  deriving (Applicative, Functor, Monad, MonadIO)

-- | Wrapper around IO actions that modify state elements created during
-- initialization.
newtype Hook a = Hook (Snaplet a -> IO (Either Text (Snaplet a)))

newtype LensT b v s m a = LensT (RST (ALens' b v) s m a)
  deriving ( Monad
           , MonadTrans
           , Functor
           , Applicative
           , MonadIO
           , MonadPlus
           , Alternative
           , MonadReader (ALens' b v))

newtype RST r s m a = RST { runRST :: r -> s -> m (a, s) }
```

> Snaplet `Initializer`s serve dual purpose as both _initializers_ and _reloaders_. Reloads are triggered by a special handler that is bound to the `/admin/reload` route.

```haskell
-- | the first 3 arguments are snaplet's internal bookkeeping 
-- data
makeSnaplet :: Text                 -- ^ default is of the new snaplet
            -> Text                 -- ^ human readable description
            -> Maybe (IO FilePath)  
            -- ^ snaplet's root directory, i.e. where
            -- config & data files are installed
            -- `Nothing` if no data file is needed
            -> Initializer b v v    
            -- ^ how to initialize user state of the snaplets
            -> SnapletInit b v      -- ^ init snaplet of type v  

nestSnaplet :: ByteString  
            -- ^ root url for all the snaplet's routes
            -> SnapletLens v v'     -- ^ pointer to sub-snaplet
            -> SnapletInit b v'     -- ^ init sub-snaplet
            -> Initializer b v (Snaplet v')

embedSnaplet :: ByteString
             -> SnapletLens v  v'
             -> SnapletInit v' v'
             -> Initializer b v (Snaplet v')
```

## Handler

```haskell
newtype Handler b v a =
    Handler (LensT (Snaplet b) (Snaplet v) (Snaplet b) Snap a)
```
## Example

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Lens.Micro.Platform (makeLenses)

-- | the base state, i.e. the top most app state
data App = App 
  { _db  :: Snaplet Prostgres
  , _foo :: Snaplet Foo
  , _bar :: Snaplet Bar
  }

data Foo = Foo { _quux :: Quux  }
data Quux
data Bar

$(makeLenses ''App)
$(makeLenses ''Foo)
```

# The `Heist` snaplet

The _heist snaplet_ is a snaplet dedicated for managing [[HTML template in Haskell#Heist|Heist html templates]].  

```haskell
-- module : Snap.Snaplet.Heist

-- | the state type for heist snaplets
-- `b` is the base state
data Heist b 
```

basically, heist snaplet provides the functionality of
1. adding heist templates and config where to load them
2. _render_ heist templates and render configuration:
    1. render methods (runtime, compile or generic)
    2. [[HTML template in Haskell#Splice|template splicing]]

"rendering" is an action of type `Handler b v ()` that you can bind to a router endpoint.

```haskell
-- module: Snap.Snaplet.Heist

-- render in `Handler b b` and compiles in `m`
type SnapHeist b m = HeistT (Handler b b) m 

-- Compiled in loading time
type SnapCSplice b = 
  SnapHeist b IO (DList (Chunk (Handler b b)))

-- Interpreted in runtime  
type SnapISplice b = SnapHeist b (Handler b b) Template

render, gRender, cRender 
  :: HasHeist b 
  => ByteString       -- ^ template name
  -> Handler b v ()
renderAs, gRenderAs, cRenderAs 
  :: HasHeist b 
  => ByteString       -- ^ response Content-Type
  -> ByteString       -- ^ template name
  -> Handler b v ()
```

## Adding splices

To initialise Heist, you need to put all your splices and templates into a `HeistConfig` and pass it to `initHeist'`.

```haskell
-- ============================================================
-- Snap.Snaplet.Heist
-- ============================================================
heistInit' :: FilePath                  -- ^ Path to templates
           -> HeistConfig (Handler b b) -- ^ Initial HeistConfig
           -> SnapletInit b (Heist b)

-- add splices at load time
addConfig :: Snaplet (Heist b) 
          -> SpliceConfig (Handler b b) 
          -> Initializer b v ()

hcSpliceConfig :: Lens' (HeistConfig m) (SpliceConfig m)
scCompiledSplices 
  :: Lens' (SpliceConfig m) (Splices (Compiled.Splice m))
```

example add splices 

```haskell
load :: MonadIO n 
     => FilePath 
     -> Splices (C.Splice n) 
     -> IO (HeistState n)
load baseDir splices = do
  tmap <- runExceptT $ ExceptT $ initHeist $ 
            emptyHeistConfig 
              & hcNamespace .~ ""
              & hcErrorNotBound .~ False
              & hcSpliceConfig .~ sc
  either (error . concat) return tmap
 where
  sc :: SpliceConfig n
  sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
              & scCompiledSplices .~ splices
              & scTemplateLocations .~ [loadTemplates baseDir]
```


# Use the blaze html template engine



# References

1. Documentation [`Snap.Snaplet`](https://hackage.haskell.org/package/snap-1.1.3.3/docs/Snap-Snaplet.html#t:Snaplet)
2. [24 days of Hackage: snap](https://blog.ocharles.org.uk/blog/posts/2012-12-19-24-days-of-hackage-snap.html)
3. [some predefined snaplets](http://snapframework.com/snaplets)


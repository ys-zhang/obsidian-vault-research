#code-reading  #Haskell  

# Main function

```haskell
shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
    addTiming "Function shake"
    -- ====== READING NOTE(ZYS): entry points ======
    --   shakeWithDatabase :: ShakeOptions
    --                     -- ^ opt used to create the db
    --                     -> Rules ()
    --                     -- ^ rules used to create the db
    --                     -> ( ShakeDatabase -> IO a )
    --                     -- ^ actions to run with db
    --                     -> IO a
    --                     -- ^ result of action
    (_, after) <- shakeWithDatabase opts rules $ \db -> do
        shakeOneShotDatabase db
        shakeRunDatabase db []
    shakeRunAfter opts after

```


# Database

the following function creates a database
```haskell
module Development.Shake.Internal.Core.Run where

open :: Rules () -> IO RunState
open rs =  do
    SRules{actions, builtinRules, userRules} <- runRules opts rs
    
    checkShakeExtra shakeExtra
    curdir <- getCurrentDirectory

    database <- usingDatabase cleanup opts diagnostic builtinRules
    (shared, cloud) <- loadSharedCloud database opts builtinRules
    pure RunState{..}

```

the follow function uses the db to build

```haskell
-- | Given an open 'ShakeDatabase', run both whatever actions were added to the 'Rules',
--   plus the list of 'Action' given here. Returns the results from the explicitly passed
--   actions along with a list of actions to run after the database was closed, as added with
--   'Development.Shake.runAfter' and 'Development.Shake.removeFilesAfter'.
shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])

```

## Entrypoints

```haskell
-- | Given some options and rules, 
--   create a 'ShakeDatabase' that can be used to run
--   executions.
shakeWithDatabase :: ShakeOptions 
                  -- ^ passed to `shakeOpenDatabase`
                  -> Rules () 
                  -- ^ passed to `shakeOpenDatabase`
                  -> (ShakeDatabase -> IO a) 
                  -- ^ action to perform with the db
                  -> IO a
                  -- ^ action result

shakeOpenDatabase :: ShakeOptions 
                  -> Rules () 
                  -> IO (IO ShakeDatabase
                        , IO ()) -- actions to close the db
shakeOpenDatabase opts rules = do
    -- ...
    -- ZYS NOTES: UseState indicates db's status
    use :: Var UseState <- newVar $ Open False False 
    -- ====== READING NOTE(ZYS): Action creates the db  ======
    let runState :: IO RunState  = open cleanup opts (rules >> defaultRules)
    let mkDb :: IO ShakeDatabase = ShakeDatabase use <$> runState
    -- ====== READING NOTE(ZYS): Action creates the db  ======
    alloc :: IO ShakeDatabase
    let alloc = withOpen ...  $ 
    -- ...
    pure (alloc, free)

```

## The database Types 

In essence, a `ShakeDatabase` is a map from `Key` to a `Value` decorated with meta info for minimising rerun, for detail see the `Result v` type

```haskell
data ShakeDatabase = ShakeDatabase (MVar UseState) RunState

data RunState = RunState
    {opts :: ShakeOptions
    ,builtinRules :: Map.HashMap TypeRep BuiltinRule
    ,userRules :: TMap.Map UserRuleVersioned
    ,database :: Database
    ,curdir :: FilePath
    ,shared :: Maybe Shared
    ,cloud :: Maybe Cloud
    ,actions :: [(Stack, Action ())]
    }

type Database = DatabasePoly Key Status

-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijection.
--   There may be dangling Id's as a result of version changes.
--   Lock is used to prevent any torn updates
data DatabasePoly k v = Database
    {lock :: Lock
    ,intern :: IORef (Intern k) 
    -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (k, v) 
    -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> k -> v -> IO () 
    -- ^ Record all changes to status
    ,vDefault :: v
    }
```


```haskell
-- We deliberately avoid Typeable instances on Key/Value 
-- to stop them accidentally being used inside themselves
data Key = forall a . Key
    {keyType :: TypeRep
    ,keyShow :: a -> String
    ,keyRnf :: a -> ()
    ,keyEq :: a -> a -> Bool
    ,keyHash :: Int -> a -> Int
    ,keyValue :: a
    }

data Value = forall a . Value
    {valueType :: TypeRep
    ,valueShow :: a -> String
    ,valueRnf :: a -> ()
    ,valueValue :: a
    }


data Status
    = Ready !(Result (Value, OneShot BS_Store)) 
    -- ^ I have a value
    | Failed !SomeException !(OneShot (Maybe (Result BS_Store))) 
    -- ^ I have been run and raised an error
    | Loaded !(Result BS_Store) 
    -- ^ Loaded from the database
    | Running !(NoShow (Either SomeException (Result (Value, BS_Store)) -> Locked ())) (Maybe (Result BS_Store)) 
    -- ^ Currently in the process of being checked or built
    | Missing 
    -- ^ I am only here because I got into the Intern table
      deriving Show


data Result a = Result
    {result :: !a 
     -- ^ the result associated with the Key
    ,built :: {-# UNPACK #-} !Step 
     -- ^ when it was actually run
    ,changed :: {-# UNPACK #-} !Step 
     -- ^ the step for deciding if it's valid
    ,depends :: ![Depends] 
     -- ^ dependencies (don't run them early)
    ,execution :: {-# UNPACK #-} !Float 
     -- ^ how long it took when it was last run (seconds)
    ,traces :: ![Trace] 
     -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving (Show,Functor)

```


## Run database



# Rules & Action

TLDR; How rules are specified/generated.

```
Action ---- action ---> Rules () ----runRules----> SRules ()
```

## `Rules` and `SRules`
There are 2 types connected to the _Rule_ concept
1. `Rules`
2. `SRules`
`SRules` is a data type specifies pure rule specification, and its side-effect companion is the `Rules` type. 

Basically, `Rules a :~: ShakeOptions -> SRules {- bf mod -} -> IO a {- aft mod -}`

```haskell
data SRules list = SRules
    {actions :: !(list (Stack, Action ()))
    ,builtinRules :: !(Map.HashMap TypeRep{-k-} BuiltinRule)
    ,userRules :: !(TMap.Map UserRuleVersioned)
    ,targets :: !(list Target)
    ,helpSuffix :: !(list String)
    ,allowOverwrite :: Bool
    }

-- this is just a function 
--   (ShakeOptions, IORef (SRules ListBuilder)) -> IO a
newtype Rules a = 
  Rules (ReaderT (ShakeOptions, IORef (SRules ListBuilder)) IO a) 
  -- ^ All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, Control.Monad.Fail.MonadFail)

```

```haskell
newRules :: SRule ListBuilder -> Rules ()
newRules x = Rules $ liftIO . flip modifyIORef' (<> x) =<< asks snd


runRules :: ShakeOptions -> Rules () -> IO (SRules [])
runRules opts (Rules r) = do
    (ref :: IORef (SRules ListBuilder)) <- 
        newIORef mempty{allowOverwrite = shakeAllowRedefineRules opts}
    runReaderT r (opts, ref)
    -- ^ ZYS run IO actions of the input rules, the action may modifies ref
    SRules{..} <- readIORef ref
    pure $ SRules (runListBuilder actions) builtinRules userRules (runListBuilder targets) (runListBuilder helpSuffix) allowOverwrite

```


## the action type

```haskell
newtype Action a = Action {
    fromAction :: RAW ([String],[Key]) [Value] Global Local a
  } deriving (
      Functor, Applicative, Monad, 
      MonadIO, Typeable, Semigroup, 
      Monoid, MonadFail
    )

-- ====== READING NOTE(ZYS): Type RAW parameters ======
--   ROW k v ro rw a
--   1. ROW k v ro rw :: Type -> Type is a monad 
--   2. its a (ReaderT ro), ro is readonly
--   3. its a (StateT rw),  rw is read & write
-- 
--   basically RAW is an AST that represents
--   `ReaderT ro (StateT rw IO) a` with some ability 
--   to read `v` based on `k` 
-- ====== READING NOTE(ZYS): Type RAW parameters ======
data RAW k v ro rw a where
    Fmap :: (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Ap :: RAW k v ro rw (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Next :: RAW k v ro rw a -> RAW k v ro rw b -> RAW k v ro rw b
    Bind :: RAW k v ro rw a -> (a -> RAW k v ro rw b) -> RAW k v ro rw b
    CatchRAW :: RAW k v ro rw a -> (SomeException -> RAW k v ro rw a) -> RAW k v ro rw a
    -- The following are basecases of the Inductive data type
    StepRAW :: k -> RAW k v ro rw v  
    -- ^ ZYS NOTES: here is where k-v shows up
    Pure :: a -> RAW k v ro rw a
    LiftIO :: IO a -> RAW k v ro rw a
    PutRW :: !rw -> RAW k v ro rw ()
    ModifyRW :: (rw -> rw) -> RAW k v ro rw ()
    CaptureRAW :: Capture (Either SomeException a) -> RAW k v ro rw a
    GetRO :: RAW k v ro rw ro
    GetRW :: RAW k v ro rw rw
```

`RAW` basically is an AST, to understand what `RAW` is we needs to now how the AST is evaluated:

```haskell
-- ZYS NOTES: this function name has a similar smell like
--            `go` in Golang:
--          - `go function(){ ... }`
--         V.S.
--          - `goRAW ... $ someFunction`
--                    ^ the dots sets up stack/namespace etc
--                      for the function execution
goRAW :: forall k v ro rw a . ([k] -> RAW k v ro rw [v]) 
      -> Steps k v     -- Steps k v is a mutable k-v assoc list
      -> IORef (SomeException -> IO ()) 
      -> ro 
      -> IORef rw 
      -> RAW k v ro rw a 
      -> Capture a   -- returns a continuition

```

user create using the DSL to create codes represented as `Action`s, and the function `action` create `Rules` which stores the actions waiting to be run.

```haskell
-- | stores action in a rule set
action :: Action a -> Rules ()
```


## Run actions

```
shake ->
  shakeRunDatabase ->
    run :: RunState -> Bool -> [Action ()] -> IO [IO ()]
```

# ? Plugins: `shakeExtra`

remind in the `Development.Share.Internal.Core.Run.open` function we have the following lines:

```haskell
-- the following code is simplified 
open ShakeOptions { shakeExtra, .. } = do 
  -- some action
  checkShakeExtra shakeExtra
  -- some other stuff

data ShakeOptions = ShakeOptions {
  ..
  shkeExtra :: HashMap TypeRep Dynamic
  ..
}
```

>[!note] Generic Programming
> In Haskell generic programming can be done using 3 


# Misc.

## Thread Pooling

In Shake tools for ThreadPool is defined in module `General.Pool`

the main entry point for run a collection of jobs using a Pool is 
```haskell
runPoll :: Bool  -- ^ deterministic or not
        -> Int   -- ^ num of threads in the pool
        -> (Pool -> IO()) -- ^ action to run
        -> IO ()
```


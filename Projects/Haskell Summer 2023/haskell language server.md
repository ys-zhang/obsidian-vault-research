#Haskell 
#code-reading 
#Haskell-Summer-2023 
#lsp

# Logger 
```haskell
{-# LANGUAGE RankNTypes #-}
import qualified Data.Text as T

newtype Logger = Logger 
  { logPriority :: Priority -> T.Text -> IO () }
-- | `Recorder` generalizes `Logger` 
--   from `Text` to `msg` and `IO` to `m` 
newtype Recorder msg = Recorder
  { logger_ :: forall m. (MonadIO m) => msg -> m () }
  -- ^ This needs `RankNTypes` extension

logWith :: (MonadIO m, HasCallStack) 
  .     => Recorder (WithPriority msg) 
        -> Priority 
        -> msg 
        -> m ()

{- Create Recorders -} 
textHandleRecorder :: Handle -> Recorder T.Text
mkDefaultHandleRecorder 
  :: MonadIO m
  => Maybe [LoggingColumn]  -- content & format
  -> Lock 
  -> Handle
  -> m (Recorder (WithPriority (Doc a)))   

makeDefaultStderrRecorder 
  :: MonadIO m 
  => Maybe [LoggingColumn] 
  -> m (Recorder (WithPriority (Doc a)))

-- | If no path given then use stderr, otherwise use file.
withDefaultRecorder
  :: MonadUnliftIO m
  => Maybe FilePath
  -- ^ Log file path. `Nothing` uses stderr
  -> Maybe [LoggingColumn]
  -- ^ logging columns to display. `Nothing` uses `defaultLoggingColumns`
  -> (Recorder (WithPriority (Doc d)) -> m a)
  -- ^ action given a recorder
  -> m a
  
-- | Given a 'Recorder' that requires an argument, produces a 'Recorder'
-- that queues up messages until the argument is provided using the callback, at which
-- point it sends the backlog and begins functioning normally.
withBacklog :: (v -> Recorder a) -> IO (Recorder a, v -> IO ())
```

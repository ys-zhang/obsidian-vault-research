#Haskell #code-reading 


# Problem

`liftIO` cannot be used when the `MonadIO m` is in some negative position.

```haskell

readFile :: FilePath -> IO ByteString 

readFileM :: MonadIO m => FilePath -> m ByteString  
readFileM = liftIO . readFile

-- however, the following function cannot be lifted
withFile :: FilePath -> (ByteString -> IO ()) -> IO ()
-- withFileR cannot be implemented only use `withFile` and `liftIO`
withFileR :: FilePath 
          -> (ByteString -> ReaderT Env IO ()) 
          -> ReaderT Env IO ()
```

# Solution : `MonadUnliftIO`

```haskell
class MonadIO m => MonadUnliftIO where 
  withRunInIO :: ( (forall a . m a -> IO a) -- UnliftIO m
                    -- ^ the unliftIO function
                    --   also dubbed runInIO
                   -> IO b
                  )
              -- ^ call a callback in IO 
              --   the callback provides 
              --   the ability to run 
              --   arbitrary `m` action 
              --   in IO
              -> m b
              -- ^ get the result in `m`
  withRunInIO inner = do 
    unliftIO <- askUnliftIO 
    ans <- liftIO $ inner unliftIO 
    return ans 
  askUnliftIO :: m (UnliftIO m) 

type UnliftIO m = forall a. m a -> IO a
```

> `withRunInIO` provides a function to run arbitrary `m` computations in `IO`


```haskell
withFile :: FilePath -> (ByteString -> IO ()) -> IO ()
withFileR :: FilePath 
          -> (ByteString -> ReaderT Env IO ()) 
          -> ReaderT Env IO ()
withFileR fp ma = 
  withRunInIO $ 
    \unliftIO -> 
      let a = unliftIO . ma
      in  withFile fp a

instance MonadUnliftIO m => MonadUnliftIO (ReaderT e m) where 
  withRunInIO inner = 
    ReaderT $ \r ->
      withRunInIO $ \unliftIO -> 
        inner (unliftIO . flip runReaderT r)
```


## askUnliftIO




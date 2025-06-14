#academic-paper #reading-note 

# Dynamic method

```haskell
-- | dynamic security level
data DynSL = L | H deriving (Show, Eq, Ord)

-- | Sabelfeld and Russo’s monitor as a monad tranformer
-- runtime flow analysis of security level requirement 
-- of a single action
newtype DynFlowSabT m a = DynFlowSabT 
    { runDynFlowSabT 
        :: DynSL      -- ^ lvl before the action
        -> m ( DynSL  -- ^ lvl after the action
             , a
             )
    } 
instance Monad m => Monad (DynFlowSabT m) where
  return a = DynFlowSabT $ \pc -> return (pc, a)
  ma >> mb = DynFlowSabT $ \pc -> 
    runDynFlowSabT ma pc
      >> runDynFlowSabT mb pc
  ma >>= f = DynFlowSabT $ \pc -> do 
      (pc0, a) <- runDynFlowSabT ma pc
      runDynFlowSabT $ f a $ max pc0 pc
      
```

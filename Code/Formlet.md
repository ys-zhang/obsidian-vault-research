#Haskell  #applicative-functor 

>[!tldr]
> the idea of formlet is based on the observation that 
> 1. the `view` or `render` of a form is an [[Applicative Functor]];
> 2. the `parsing` or `yields` or `evaluate` of a form is also an Applicative functor.
> 3. Applicative functors are [[Applicative Functor#Composing Applicative functors|composable]]

# 1 Design space

We want to capture the notion of a part of a form, specifically a part for _collecting values of a given type_ or purpose; we call such an abstraction a **formlet**. 

Given a desired type $A$, we have to make a form that responsible for setting value $a$ of type $A$:
1. The designer of the formlet should choose the presentation $p: P$ of value $a:A$, and decide how to parse the presentation to a value of the desired type. In other words, 
    $$\begin{align} view &: A \to P \\ parse &: P \to A \end{align}$$
2. Clients of the formlet should be _insulated_ from the choice of presentation ($view$), and also from the calculation that yields the abstract value ($parse$). 
3. We should be able to _compose_ formlets to build larger formlets.

>[!note] Terminology difference
>The paper use different terminology where
> - _body_ is `view`
> - _yields_ is `parse`
>
>The paper uses _idioms_ as name for _applicatives_


# 2 A simple implementation

```haskell
newtype Formlet v env a = Formlet 
  { runFormlet :: Int        -- ^ used for gen id of html form
               -> ( v        -- ^ rendered html
                  , env -> a -- ^ extract value from submission
                  , Int)     -- ^ updated id counter 
  }
  deriving Functor

newtype View v a =  -- notice `a` is a phantom type param
  View { runView :: Int -> (v, Int) }
  deriving Functor
  
newtype Extract env a = 
  Extract { runExtract :: Int -> env -> a }
  deriving Functor

splitFormlet :: Formlet v env a -> (View v a, Extract env a)
splitFormlet f = (View v, Extract e)
 where 
  v = (\(v, _, s) -> (v, s)) . (runFormlet f)
  e = (\(_, e, _) -> e) . (runFormlet f)

mergeViewExtract :: View v a -> Extract env a -> Formlet v env a
mergeViewExtract (View view) (Extract extract) = 
  Formlet $ \s -> let (v, s') = view s
                  in  (v, extract s, s')

prop_merg_split = 
  (uncurry mergViewExtract . splitFormlet) === id
prop_split_merg = 
  (splitFormlet . uncurry mergViewExtract) === id
```

1. its easy to see `Int -> (v, Int)` is _monoid_ if `v` is a monoid, which makes `View v` an _monoid accumulator_ whose effects are accumulating the monoid value.
2. `Extract env` is an _applicative functor_.
Thus we can compose the 2 applicative to make `Formlet v env` an instance of applicative 

```haskell
instance Monoid v => Applicative (View v) where
  pure _ = View $ \s -> (mempty, s)
  mf <*> mx = let View f = mf
                  View x = mx
              in  View $ \s -> let (vf, s')  = f s
                                   (vx, s'') = x s''
                               in  (vf <> vx, s'')

instance Applicative (Extract env) where
  pure x = Extract $ \s e -> x
  (Extract mf) <*> (Extract mx) = 
    Extract $ \s e -> (mf s e) (mx s e)

instance Monoid v => Applicative (Formlet v env) where
  pure x = mergeViewExtract (pure x) (pure x)
  mf <*> mx = let (vf, ef) = splitFormlet mf 
                  (vx, ex) = splitFormlet mx
              in mergeViewExtract 
                   (vf <*> vx)
                   (ef <*> ex)
```

## 2.1 deriving applicative for formlet using applicative composition

```haskell
type Namer v = Int -> (v, Int)
type ViewWriter v a = (v, a)
type Extracter env a = env -> a

type Formlet v env a = Namer (ViewWriter v (Extractor env a))
```
- `Namer v = State Int v` is an applicative 
- `ViewWriter v a` is a monoid accumulator and thus applicative if `v` is monoid
- `Extractor env a = Reader env a` is an applicative
and thus `Formlet` as a composition/stack of applicative is itself an applicative.

## 2.2 composing formlets

```haskell
mapView :: (v -> v') -> Formlet v env a -> Formlet v' env a
mapView f =  
      splitFormlet 
  >>> first ( View . (\arr -> arr >>> first f ). runView) 
  >>> uncurry mergeViewExtract
  
composeFormlet :: forall v env a b c
                . Monoid v
               => (a -> b -> c)      -- ^ data constructor
               -> (v -> v -> v)      -- ^ view template
               -> Formlet v env a 
               -> Formlet v env b 
               -> Formlet v env c 
composeFormlet cons temp fa fb = 
  let fc' = pure cons <*> fa' <*> fb'
      app :: (v -> v) -> v
      app f = f mempty
  in  mapView (app . unOpHomo) fc'
 where 
  fa' :: Formlet (OpHomo v) env a
  fa' = mapView (OpHomo . temp) fa
  fb' :: Formlet (OpHomo v) env b
  fb' = mapView (OpHomo . const) fb

newtype OpHomo a = OpHomo {unOpHomo :: a -> a }

instance Semigroup (OpHomo a) where 
  (OpHomo f) <> (OpHomo g) = OpHomo (g >>> f) 
instance Monoid (OpHomo a) where 
  mempty = OpHomo id
```

> full code:  https://play.haskell.org/saved/P4M80lRX

## 2.3 add input validation

```haskell
type Formlet' v env a = Formlet v env (Maybe a)
```

since `Maybe` is an applicative, and `Formlet' v env` is the composition of `Formlet v env` and `Maybe`, we have `Formlet' v env` is an applicative. 

```haskell
check :: (a -> Maybe a) -> Formlet env v a -> Formlet' env v a
check p fm = p <$> fm

check' :: (a -> Maybe a) -> Formlet' env v a -> Formlet' env v a
check' p fm = join @Maybe <$> ((fmap @Maybe p) <$> fm)
```

# 3 The `digestive-functors` implementation 
#todo 

```haskell
type Formet v m a 
  =  Maybe a        -- ^ default value
  -> Form v m a 

type Form v m a = FormTree m v m a
```

the library uses a [Free monad](https://serokell.io/blog/introduction-to-free-monads) approach.
```haskell
-- | Embedded tree structure for forms 
-- the basis for deferred evaluation
-- and the applicative interface.
data FormTree t v m a where
    -- Setting refs
    Ref :: Ref -> FormTree t v m a -> FormTree t v m a

    -- Applicative interface
    Pure :: Field v a -> FormTree t v m a
    App  :: FormTree t v m (b -> a)
         -> FormTree t v m b
         -> FormTree t v m a

    -- Modifications
    Map  :: (b -> m (Result v a)) 
         -> FormTree t v m b 
         -> FormTree t v m a
    Monadic  :: t (FormTree t v m a) -> FormTree t v m a

    -- Dynamic lists
    List :: DefaultList (FormTree t v m a)  -- Not the optimal structure
         -> FormTree t v m [Int]
         -> FormTree t v m [a]

    -- Add arbitrary metadata. This metadata applies to all children.
    Metadata :: [Metadata] -> FormTree t v m a -> FormTree t v m a
    
```


```haskell
-- | set a name for a form
(.:) :: Text -> Form v m a -> Form v m a
```

## 3.1 the `view` applicative functor

the `view` part of the formlet is defined in seperate packages for each template engine, here use [[HTML template in Haskell#Blaze]].

```haskell
module Text.Digestive.Blaze.Html5 where
inputText :: Text 
          -- name of the form
          -> View Html 
          -> Html
```


# 4 References

- [Haskell implementation: digestive-functors](https://hackage.haskell.org/package/digestive-functors)
- [Racket implementation](https://docs.racket-lang.org/web-server/formlets.html)
- [Beyond the Monad fashion (II.): Creating web forms with LINQ](https://tomasp.net/blog/formlets-in-linq.aspx/)
- [24 Days of Hackage: digestive-functors](https://blog.ocharles.org.uk/blog/posts/2012-12-02-digestive-functors.html)
- Paper: The Essence of Form Abstraction
- https://jaspervdj.be/posts/2012-03-21-digestive-functors-0.3.html
- https://github.com/jaspervdj/digestive-functors/blob/master/examples/tutorial.lhs

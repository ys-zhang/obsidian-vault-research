# 1 Intro: CSP in JS

In the style of **continuation passing style**, you are _not allowed to return values_, and hence you must resort to passing continuations around.

```js
function pythagoras(x, y, cont) { 
  square(x, (xs) => {
    square(y, (ys) => { 
      add(xs, ys, cont)
    })
  })
} 

function square(x, cont) {
  cont(x * x)
}

function add(x, y, cont) { 
  cont(x, y)
}
```

There are however two problems with CPS: 
1. Passing around continuations increases the size of the call stack. 
2. It's a pain to write nested functions.

For the 1st problem, the _continuation_ that passed into the function are called at the _tail position_ of the caller. Thus we can use tail optimisation that drop the caller's stack frame before call in to the continuation.

The 2nd problem is usually solved by a function called **call-with-current-continuation** which is often named as `callcc` 

# 2 CSP in Haskell

First let us consider the type of functions in CPS in Haskell, one basic problem is what is the return type of a function written in CPS?

```haskell
add :: Int -> Int -> Int
add x y = x + y

add_cps x y cont = cont (x + y)
```

type inference gives the type of `addCSP`
```haskell
add_cps :: Int -> Int -> (Int -> r) -> r
```
unlike `add`, `add'` returns `forall r . (Int -> r) -> r`, follow Haskell, lets give it name it `Cont r Int`

> Thus, you can adopting CSP by change the returned type from `a` to `type Cont r a = (a -> r) -> r`. 


_but wait CPS forces no return from any function, but we have this `Cont r a`, it seems violates the rules_ 

>[!important] Suspension points
> The name of type `Cont r a` is quite misleading.
>
> It does **not** corresponding to the concept of _Continuation_, what is really represents is a _Suspension point_ which awaiting for a  value of type `a -> r`, and that value is the real _Continuation_. 
>
> To be more specific
> ```haskell
>  data SuspensionPoint r a where
>    SuspensionPoint 
>      :: { runSp :: (a -> Suspension r b)  -- ^ continuation
>                 ->  Suspension r a 
>         } 
>      -> Suspension r a
>    SpPure :: r -> SuspensionPoint r r
> ```

Thus programming in CPS is all about _chaining suspension points_:

```haskell
type Cont r a = (a -> r) -> r

runCont :: Cont r a -> (a -> r) -> r
runCont supensionPoint cont = suspensionPoint cont

pathagoras_cps :: Int -> Int -> Cont r Int
pathogoras_cps x y = 
  let s_xsquare = square_cps x
      s_ysquare = square_cps y
  in  \cont -> 
        runCont s_xsquare $ \xsquare -> 
          runCont s_ysquare $ \ysquare -> 
          runCont (add_cps xsquare ysquare) cont

add_cps :: Int -> Int -> Cont r Int
add' x y = Cont $ \cont -> cont $ x + y

square_cps :: Int -> Cont r Int
square_cps x = Cont $ \cont -> cont $ x * x

```

to solve the deep nesting of `runCont` we need to compose suspension points

```haskell
newtype Cont r a = 
  Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where 
  fmap f cont = Cont $ \c -> runCont cont (c . f)

instance Applicative (Cont r) where 
  pure = return 
  mf <*> mx = mf `ap` mx 
  
instance Monad (Cont r) where 
  return x = Cont $ \c -> c x
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b 
  ma >>= mf = Cont $ \br -> 
    runCont ma $ \a -> 
      let cb = mf a 
      in  runCont cb br

pathogoras'' :: Int -> Int -> Cont r Int 
pathogoras'' x y = do 
  sx <- square' x 
  sy <- square' y 
  add' sx sy
```

> the `r` type parameter in `cont r a` is the type of the only value you can extract from the monad, and the type `a` is the type of value that is already known, in some CSP function `f :: b -> cont r a` is the actual returned type of `f`

```haskell
-- extracting value from continuation
evalCont :: Cont r r -> r
evalCont cont = runCont cont id
```

# 3 callCC 

>[!tldr]
> `callCC` helps you acquire the _current continuation_ in a abstract _suspension point_.
>
> Basically, it lets you do **early return**.

![[Pasted image 20250106101114.png]]
- $E$ represents the current continuation
- `call_cc` is a function that takes a function $f$, when evaluated, will call $f$ with a function which is the reified current continuation $E$.

while in delimited continuation: 
![[Pasted image 20250106102827.png]]

![[Pasted image 20250106103244.png]]




```haskell
class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a
```

before diving into `callCC`, we need to clarify several concepts.

## 3.1 current continuation

In CPS, function does not return value, but _suspension points_(`Cont r a`). 

When call a CPS function, the _continuation_ that will be supplied to the _about to returned_ suspension point is dubbed as _current continuation_.

The problem is that, _suspension point_ can be modelled in different ways, given an abstract _suspension point_ type, how can we acquire the _current continuation_? 

## 3.2 semantics of `callCC`

`callCC` gives you access to _current continuation_ and lets you call **into** it, 

Here "into" means the function "exits", i.e., the function's context(stack frame) is discarded and the control of the machine is handle to the continuation. 

The "into" also means we can early return from the function.

```haskell
foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    -- the following will not be executed if (y>20)
    return (show $ y - 4)
```

## 3.3 implementation of `callCC`

```haskell

instance MonadCont (Cont r) where
  callCC :: (  (a -> Cont r b)   -- ^ continuation
            -> Cont r a
            ) 
         -> Cont r a
  callCC f = Cont $ \cc -> 
    let k a = Cont (\_ -> cc a)
    in  runCont (f k) cc
  
```

```haskell

bar :: (Int -> Cont r b) -> Cont r Int
bar k = do 
  k 0 
  -- the follow represents the continuation supplies to 
  -- k 0, however (k 0) will not call the following continuation
  -- but call cc with 0
  return 1

-- when used in callCC 
-- (k 0) c
bar k = Cont $ \cont -> 
  let g _b = cont 1   -- return 1
  in  runCont (k 0) 
```

[great examples in Haskell wiki](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#Example:_a_complicated_control_structure)

<iframe src="https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#Example:_coroutines" width="100%" height="600"> 
</iframe>

# 4 Implementation 


## 4.1 By Desugar

 The core language

```haskell
data Lit
type Id = String
data Expr 
  = Lit Lit 
  | Var Id
  | Lam Id Expr 
  | App Expr Expr
  -- binding
  | Let Id Expr Expr
  -- for mutation
  | Seq Expr Expr
  | Box Expr
  | UnBox Expr
  | SetBox Expr Expr

-- | notice that the cps function make an expr of type 
-- `           t1 -> t2 -> ... tn -> a`
-- to 
-- `forall b . t1 -> t2 -> ... tn -> (a -> b) -> b`
cps :: Expr -> Expr
cps e@(Lit _)    = Lam "cc" $ 
  App (Var "cc") e
cps e@(Var _)    = Lam "cc" $ 
  App (Var "cc") e 
cps (Lam x body) = Lam "cc" $ 
  App (Var "cc") $ 
    Lam x $ Lam "dyn_k" $ App (cps body) (Var "dyn_k")
cps (App f x)    = Lam "cc" $ 
  App (cps f) $ Lam "f" $ 
    App (cps x) $ Lam "x" $ 
      App (App (Var "f") (Var "x")) (Var "cc") 
cps (Let v e b) = Lam "cc" $  
  App (cps e) $ Lam v $  
    App (cps b) (Var "cc")
```

Notice `Let v e b` can be desugar into `App (Lam v b) e` however is method produces more _administrative lambdas_.
```haskell
cps $ App (Lam v b) e 
==> Lam "cc" $ 
      App (cps (Lam v b)) $ Lam "f" $ 
        App (cps e) $ Lam "x" $ 
          App (Var "f") (Var "x") (Var "cc") 
==> Lam "cc" $
      let f = Lam "f" $ 
                App (cps e) $ Lam "x" $ 
                  App (Var "f") (Var "x") (Var "cc") 
      in App f (Lam v $ Lam "k" $ App (cps b) (Var "k"))
==> Lam "cc" $
      App (csp e) $ Lam v $ App (csp b) (Var "cc") 
```


## 4.2 Directly


# 5 Reference 

1. [Diff: Btw Continuation](https://stackoverflow.com/questions/14019341/whats-the-difference-between-a-continuation-and-a-callback)
2. https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
3. [Talk: Demystify continuation](https://youtu.be/TE48LsgVlIU?si=w6gmDD4VdQBCfOj1)



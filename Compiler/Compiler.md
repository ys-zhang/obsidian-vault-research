
# 1 Tricks

## 1.1 Implement binding with sugaring over anonymity

once you have _lambda expression_, you can desugar name bindings using lambda.

>the idea is to use the [[Continuation|Continuation Passing Style]]

consider the following expression
```haskell
let x = e in f x
-- ==> k for continuation
(\k -> k e) f 
-- or equivently
(\x -> f x) e
```

code for desugar
```haskell
data Expr = ...
          | Let (Id, Expr) Expr
          | Lam Id Expr
          | App Expr Expr
          
desugarLet (Let (n, v) e) = 
  App (Lam n e) v
```

# 2 Utilities

- [Compiler Explorer (godbolt.org)](https://godbolt.org/), see [[assembly]] generated from high level language.
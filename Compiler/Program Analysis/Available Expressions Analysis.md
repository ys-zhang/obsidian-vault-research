#program-analysis #compiler 

> For each _program point_, which _expressions_ must have already
> been computed, and not later modified, on _all paths_ to the program point.

```haskell
kill :: BB -> [Expr]
```
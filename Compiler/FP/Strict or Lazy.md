#Haskell 

[Youtube series](https://www.youtube.com/playlist?list=PLyzwHTVJlRc8620PjqbM0x435-6-Gi1Gu)

# Lecture 1

strictness or lazy is part of the semantics of a program, where a program is a specification of behaviour.

For instance, C standard stipulates code are strictly evaluated at least semantically, which further indicates that observably, the evaluation order matters.

```c
int foo(int a, int b) {
  int x = bar(a);
  int y = baz(b);
  return x + y;
}
```
strictness puts an evaluation order constraint on complier.

# Lecture 2 

>[!take away]
> The semantics of a program will probability be different when taking under _strict_ and _non-strict_ languages.
> However, there are cases that the semantics keeps the same

A case that _non-strict_ and _strict_ have the same semantics
```haskell
-- even though in a pure lazy non-strick language like haskell
-- the compiler cannot change the execution order of 
-- the text expr and the false branch
safeDiv :: Int -> Int -> Maybe Int
safeDiv a b = if b == 0 then Nothing else Just (a `div` b)
```

# Lecture 3: Demand


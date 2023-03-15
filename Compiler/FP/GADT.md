#type-theory 

# A non-travail example

consider the following 2 types represents a tree
```ocaml

type 'a tree = 
    Empty
  | Tree of 'a tree * 'a * 'a tree

type _ ntree = 
    EmptyN: 'a tree
  | TreeN: 'a * ('a * 'a) ntree -> 'a ntree    
```

the `ntree` must be balances by imposing a constraint through its data constructor 

> The definition of `ntree` is **non-regular** because the type constructor it defines, `ntree`, is _not uniformly applied to its type parameters_ in the definition: instead, it is instantiated with ’`a * ’a` in the argument of `TreeN`.
> 
> Allowing the **return types** of _constructors_ to vary in a similar way gives us a variety of non-regular types known as generalised algebraic data types (GADTs).


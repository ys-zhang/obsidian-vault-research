
_Backpatching_ is a method to compress compiler passes, in other words, get the same out come by less AST traversal times.

The problem is when compute an _attribute_ for some node needs the _attribute_ value of some other nodes that can only be determined later.

This is not a problem when we use a lazy language^[for instance Haskell] to implement the AST traversal.

In an imperative language, we can use pointers for the attribute value, then fill in the structure the pointer points to after the traversal finished.



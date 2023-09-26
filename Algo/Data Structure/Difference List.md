#functional-programming #data-structure #algorithm #Haskell #Prolog
# Functional representation

recall $abs$ and $rep$ functor from [[General Theory of Representation and Abstraction]].

Here the _abstract data type_ is $[a]$(the set of ordinary lists), and the _concrete data type_ is a subset of $[a] \to [a]$

```haskell
type DList a = [a] -> [a]

rep :: [a] -> DList a
rep xs = \ys -> xs ++ ys

abs :: DList a -> [a]
abs fx = fx []
```

## Append 

```haskell 
append :: DList a -> DList a -> DList a 
append fx fy = fx . fy
```
correctness is from the fact 
```haskell
abs (fx `append` fy) = (abs fx) ++ (abs fy)

rep (xs ++ ys) = (rep xs) `append` (rep ys)
```

Function composition is an efficient operation. It can always be performed in constant time, since no actual computation is involved. 

The function `rep` also takes constant time, so it remains to be shown that `abs` is reasonably efficient.

Provided that a 'functional list' is built using only `rep` and `append`, `abs` can convert it into an ordinary list in time proportional to its length.

>[!NOTE] Linear Complexity Reasoning
>a general form of a `DList` is 
>```haskell
>(++) lst1 $ (++) lst2 $ (++) lst3 ... $ (++) lstN []  
>```
>the complexity of `append xs ys` is linear in `xs` and _independent_ of `ys`.
>
>Where a bad construction example (exactly when reversing a list) is 
>```haskell
>(++) ( (++) ( (++) lst1 lst2) lst3 ) lst4
>```

Often the cost of constructing a list of length n using append is greater than this, because recursive calls of append appear as left arguments of other calls of append, and so contribute several times to the total cost. 
This can lead to a total cost proportional to the square of the length of the list being constructed. 
In such cases our "functional representation' offers a substantial improvement.

# Open List 

- An _open list_ is a list only known up to a point, or a list with a _hole_ at the tail.
- A _proper list_ is a full known list, with no holes.

For instance,
- the list `List = [a, b, c | X]` is _open_ if $X$ is _unbounded_;
- (_fill_) the list `List = [a, b, c | X], X = d` is a _proper_ list;
- (_partial fill_) `List = [a, b, c | X], X = [d|X1]`


# References

1. Hughes, R. J. M. (1986). A novel representation of lists and its application to the function “reverse.” _Information Processing Letters_, _22_(3), 141–144. [https://doi.org/10.1016/0020-0190(86)90059-1](https://doi.org/10.1016/0020-0190(86)90059-1)
2. Open Lists and Difference Lists, retrieved from https://www3.risc.jku.at/education/courses/ws2007/logic-programming/additional/OpenListsAndDifferenceLists.htm

#book  #Haskell #type-theory  #functional-programming

This is some book-reading notes of the book "Thinking with Types" by Sandy Maguire


# Fundamentals

## 1. The Algebra Behind Types

There is in fact an **algebra** behind _algebraic data types_.
We can associate each type with its **cardinality**—the number of inhabitants it has, ignoring bottoms.
If two types have the same cardinality then we can construct an **isomorphism** between the two types.

1. the _cardinality_ (remember, the number of inhabitants) of `Either a b` is the cardinality of `a` plus the cardinality of `b`.
    $$ |\text{Either} \;a \; b| = |a| + |b| $$
2. the _cardinality_ of a product type is the product of their cardinalities.
    $$ |(a,b)| = |a| \times |b| $$
3. Function types also have an encoding as statements about cardinality—they correspond to exponential.
    $$ |a \to b| = |b|^{|a|} $$
4. Subtraction corresponds to types with particular values removed, while division of a type makes some of its values equal

This _canonical representation_ of an algebraic data type is known as a _sum of products_, and refers to any type $t$ of the form,

$$
 t= \sum_m\prod_n t_{m, n}
$$

## 2. Terms, Types and Kinds

|      | term level programming         | type live programming |
| ---- | ------------------------------ | --------------------- |
| term | values/things exist at runtime | /                     |
| type | proofs about the program       | things to manipulate  |
| kind | /                              | proofs                | 

>[!note] Two meaning of the word "type"
> 1. _type_ is used to describe anything that exists at type level
> 2. _TYPE_ is the _kind_ of types that have inhabitants (concrete types)
>
>TYPE is the **kind** of things like `Int` and `Maybe Bool`—it classifies the sorts of things that exist at runtime. Some things which aren’t of kind TYPE are `Maybe` (without a type parameter), and `Show`.

There are 3 basic "kind" of kinds

| "kind" of kind | symbol       | examples       |
| -------------- | ------------ | -------------- |
| `Type` Kind    | `*`          | `Bool`         |
| Arrow Kind     | `* -> *`     | `Bool -> Bool` |
| Constraint     | `Constraint` | `Num a`        |

we also have `DataKinds` which basically lifts 
1. data constructor to type level constructor
2. type constructor to kind

for instance 

```haskell
type {- this shall be read as KIND -} Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where 
  Append '[] {- the tick lifts a data construct to type level -} ys = ys
  Append (x:xs) ys = x : Append xs ys
```

the type level function `Append` maps _type level value_ of **kind** `[a]` and a ... to a ...

>[!note] The tick
> The reason of the tick in `'[]` is that `[]` it self is a type constructor which lives in the type level.
> the _type family_ `Append` maps type level things of kind `[a]` to kind `[a] -> [a]`. 
> if we remove the tick then it is impossible to differentiate the _type constructor_ and the _lifted data constructor_ 


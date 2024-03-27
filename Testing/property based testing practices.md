#program-test #property-based-testing 



# Good practices to right effective properties


# avoid reasoning that is not checked by tests

property implementation its self may be quite complicated, for instance some property may need some predicate to be implemented which may be 
1. trivial implementation is not suitable for large test cases, e.g., too slow
2. good implementation may involve non-trivial assumptions/insights or complicate algorithm

A good practice is 
1. if you have a sophisticated implementation then test the _equivalence_ of the trivial implementation and the sophisticated one; or
2. use the trivial implementation and do not bother about this

```haskell
prop_ImplEquiv x = trivial x === sophisticated x
```

## validity testing

>[!quote]
>“Every operation should return valid results.”

```haskell
prop_Valid x = validInput x ==> validOutput (targetProgram x)
```

this property may suffer from inefficiency if the generator seldom generates valid inputs, so a dedicated generator may be in need, which indicates _shrinker shall shrink to valid inputs only_. We can test the shrinker using property:

```haskell
{- WARNING
 the precondition "validInput x" CANNOT be ommitted, 
   even given the generator only generates valid inputs.
 the reason is that once some counter example is found,
 the same buggy shrinker will be used to shrink it and 
 you may found shrinked results with invalid input and invalid 
 shrinked options
-}
-- this will report only the original input x
prop_ShrinkValid x = 
  validInput x ==> all validInput (shrink x)

-- this will report both the original input x 
-- and the result shrink option list
prop_ShrinkValid' x = 
  validInput x ==> filter (not . validInput) (shrink x) === []
```

>[!note] Summary
> Validity testing consists of defining a function to check the _invariants of your datatypes_, 
> - writing properties to test that your generators and shrinkers only produce valid results, 
> - and writing a property for each function under test that performs a single random call, and checks that the return value is valid.
>
> _since validity is about data types, it may be better to mandate it by type system_

## post conditions: intended operation behaviour

>[!def]
>A postcondition is a property that should be True after a call, or (equivalently, for a pure function) True of its result


## Metamorphic Properties

see [[Metamorphic testing]]

>[!note] Summary
> A metamorphic property tests a single function by making (usually) two related calls, and checking the expected relationship between the two results

## Inductive testing

> [!quote]
> “Inductive proofs inspire inductive tests.”

In inductive proof, given an inductive type $T$ we want to prove property $P$ holds for all $t:T$ using the following steps: 
1. $P$ holds for all $t$ constructed by the base data constructor
2. for other constructors, prove $P$ holds for all data instances constructed by this constructor if $P$ holds for instances with a smaller "size"

## Model-bases properties

>[!quote] 
>the abstraction or model can be thought of as a kind of _reference implementation_ of the program under test, though with a much simpler representation.


see [[General Theory of Representation and Abstraction]]
given an abstraction or model of the implementation testing 
``` haskell

prop_RepreCommutes x = 
  abs_program (abstract x) === abstract (program x)

```

## Other tricks

1.  equality precondition
```haskell
{-
 The 2nd propert test solves the problem of low change of k=k'
-}
prop_InsertPost k v t k' = 
  find k' (insert k v t) === if k = k' then Just v else find k' t
prop_InsertPost' k v t = 
  find k (insert k v t) === Just v
```
2. equivalence preserving
```haskell
prop_EquivPreserve x x' = 
  equivInputs x x' ==> equivOutputs (program x) (program x')
```

# Effectiveness

>[!tldr] Take-aways
> 1. Validity properties miss many bugs
> 2. Invalid test data provokes false positives.
> 3. Model-based properties are effective at finding bugs; each property tests just one operation, and finds every bug in that operation.
> 4. Postconditions are quite effective; each postcondition for a buggy operation finds all the bugs we planted in it, but some postconditions are less effective than we might expect.
> 5. Metamorphic properties are less effective individually, but powerful in combination.
>
> And
> - if time is limited, then writing model-based properties may offer the best return on investment, in combination with validity properties to ensure we don’t encounter confusing failures caused by invalid data.
> - In situations where the model is complex (and thus expensive) to define, then metamorphic properties offer an effective alternative, at the cost of writing many more properties.

# References

1. Hughes, J. (2019). How to Specify It! A Guide to Writing Properties of Pure Functions. _Trends in Functional Programming: 20th International Symposium, TFP 2019, Vancouver, BC, Canada, June 12–14, 2019, Revised Selected Papers_, 58–83. [https://doi.org/10.1007/978-3-030-47147-7_4](https://doi.org/10.1007/978-3-030-47147-7_4)
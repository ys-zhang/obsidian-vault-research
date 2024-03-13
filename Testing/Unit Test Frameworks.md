#program-test #unit-test

# Concepts 

with examples from `HUnit` a unit text frame work from Haskell

## test case and test suit
```haskell
data Test 
  = TestCase Assertion
  | TextList [Test]
  | TextLabel String Text

-- | When an assertion is evaluated, 
-- it will output a message if and only if the assertion fails.
type Assertion = IO ()
```

- A _test case_ is just a `Assertion` or a boolean value with some side effects, equivalent to an `IO ()` white can _throw exceptions_.
- A _test suit_ is just a collection of _test case_, probably labeled.

## A Language that covers some kind of logic

this is just some primitives or combinators that can build an `Assertion` from some value of type `a`

$$ f: A \to \mathrm{Assertion} $$

For unit test, the logic is _propositional logic_, i.e., quantifiers like _for all_ and _exists_ are not supported.

| name            | operator                                       | type                  | description                |
| --------------- | ---------------------------------------------- | --------------------- | -------------------------- |
| `assertFailure` |                                                | `Msg -> IO a`         | just fail with the message |
| `assertEqual`   | expected `@=?` actual or actual `@?=` expected | `a -> a -> Assertion` | fail if not equal          |
| `assertBool`    |                                                | `Bool -> Assertion`   | fail if false              |


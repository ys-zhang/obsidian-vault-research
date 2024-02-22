#Haskell  #functional-programming #Monad 


```haskell
class Applicative f => Alternative f where
  -- | identity of `<|>`
  empty :: f a  
  -- | an assoc binary operator
  -- empty <|> ma = ma
  -- ma <|> empty = empty
  (<|>) :: f a -> f a -> f a
  
  -- | some: one or more
  some :: f a -> f [a]
  some v = (:) <$> v <*> many v
  -- | many: zero or more
  many :: f a -> f [a]
  many v = some v <|> pure []

-- Conditional failure of Alternative computations.
guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

-- | Combines lists by concatenation, starting from the empty list.
instance Alternative [] where
    empty = []
    (<|>) = (++)
```


# Model Nondeterminism

> A nondeterministic programming language is a language which can specify, at certain points in the program (called "choice points"), various alternatives for program flow. Unlike an if-then statement, the method of choice between these alternatives is not directly specified by the programmer; the program must decide at run time between the alternatives, via some general method applied to all choice points. A programmer specifies a limited number of alternatives, but the program must later choose between them. 

Use List to model all options at a choice point, where we can do back tracking. 

The bind operation of list monad models this well.

# References

1. [What happens when a Haskell programmer writes Java](https://youtu.be/bPyR1ttdE7o?si=OT96IiTHU4HVAtBs)
2. [Wiki - Nondeterministic Programming](https://en.wikipedia.org/wiki/Nondeterministic_programming)
3. https://www.reddit.com/r/haskell/comments/esx7cs/why_is_list_considered_nondeterministic/
4. [Effects Without Monads: Non-determinism Back to the Meta Language](https://okmij.org/ftp/tagless-final/nondet-effect.html#structure)
5. [How to restrict a monad without breaking it](https://okmij.org/ftp/Haskell/set-monad.html#introduction)
6. [The concept of nondeterminism: its development and implications for teaching](https://dl.acm.org/doi/10.1145/1595453.1595495)


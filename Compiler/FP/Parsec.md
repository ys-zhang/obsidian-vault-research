The basic idea of a **parser** is that it takes an input (for example, a string), it consumes the characters of this string until it’s done with what it’s supposed to parse and then _pass the remaining (unparsed) string along_, which might be used as input to subsequent parsers.

```haskell
-- | takes a string, consumes part of it 
-- and get some result, then pass remaining 
-- unconsumed string along
type Parser a = String -> (a, String)
```

A more general case:

```haskell
data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

-- | i : token 
--   e : error
--   a : parse result
newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }


instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)
  
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')


instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest


instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]
  -- if left fails then try right
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)
```

# Constructors

```haskell
satisfy :: (i -> Bool) -> Parser i e i 
satisfy pred = Parser $ 
  \case
    [] -> Left [EndOfInput]
    (x:xs) | pred x -> Right (x, xs)
           | otherwise -> Left [Unexpected x]

char c = satisfy (== c)

string :: Eq i => [i] -> Parser i e [i] 
string = traverse char
```

# References

https://serokell.io/blog/parser-combinators-in-haskell

http://jakewheat.github.io/intro_to_parsing

https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing


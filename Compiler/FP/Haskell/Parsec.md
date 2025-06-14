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

# Indentation sensitive parsing

https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/indentation-sensitive-parsing.md

>[!note] logical model
> 1. Top level items are not indented
> 2. All indented tokens are directly or indirectly are “children” of some top-level definition.
> 3. there are 2 types of tokens:
>     1. indent tokens (spaces)
>     2. indented tokens
>     3. reference tokens (not spaces)
> 4. indentation level, number of indent tokens on a line
> ```
>        ----- reference token
>        |
>        v
>        xxxxxxxxxxx    
>        ␣␣␣␣aaaaaa
>        ␣␣␣␣bbbbbb
>             ^
>             |
>             ------- indented tokens
> ```


```haskell
noIndent :: MonadParsec e s m
         => m ()      -- ^ indent token (white spaces) consumer
         -> m a       -- ^ parse the data
         -> m a

indentBlock :: (MonadParsec e s m, Token s ~ Char)
            => m ()    
            -- ^ How to consume indentation (newline include)
            -> m (IndentOpt m a b) 
            -- ^ How to parse reference token
            -- and give a result of how to 
            -- parse indented tokens
            -> m a

data IndentOpt m a b
  = IndentNone a
  -- ^ parse no indented tokens, rust return the value
  | IndentMany (Maybe Int) ([b] -> m a) (m b)
  -- ^ zero or more indented tokens
  -- parameters 
  --   - (Maybe Int)  expected indentation level, 
  --                  if nothing use level 
  --                  of 1st indented token
  --   - ([b] -> m a) combine parsing results of indented tokens
  --   - (m b)        how to parse an indented token 
  | IndentSome (Maybe Int) ([b] -> m a) (m b)
  -- ^ one or more indented tokens
```
## Line folding

> A _line fold_ consists of several elements that can be put on one line or on several lines as long as the indentation level of the subsequent items is greater than the indentation level of the first item.

```
aaaaaaaaaaaaaaaaaaaaaaaaaa
  bbbbbbbbbbbbbbbbbbbbbbbb
   cccc
   
xxxxxxxxxxxxxxxx
```
line folding will treat the `b` sequence and `c` sequence as part of the `a` line, while the `x` sequence itself is a different line.

## Indent primitive combinators

```haskell
-- the current token's column number
indentLevel :: m Pos 
indentLevel = sourceColumn <$> getPosition

indentGuard :: MonadParsec m
            => m ()     -- ^ indentation (whitespace) consumer
            -> Ordering -- ^ the expected order of real lvl and ref lvl
            -> Pos      -- ^ reference indentation level
            -> m Pos    -- ^ real indentation level 
indentGuard sc ord refLvl = do 
  sc 
  realLvl <- indentLevel
  if ord == realLvl `compare` refLvl 
    then return realLvl
    else fail "incorrect indentation ..."
```

## implementation of `indentBlock`

```haskell
noIndent :: m () -> m a -> m a
noIndent sc parser = indentGuard sc EQ pos1 *> parser 

indentBlock sc r = do
  sc
  ref <- indentLevel
  a <- r
  case a of
    IndentNone x -> x <$ sc
    IndentMany indent f p -> do
      mlvl <- (optional . try) (C.eol *> indentGuard sc GT ref)
      done <- isJust <$> optional eof
      case (mlvl, done) of
        (Just lvl, False) ->
          indentedItems ref (fromMaybe lvl indent) sc p >>= f
        _ -> sc *> f []
    IndentSome indent f p -> do
      pos <- C.eol *> indentGuard sc GT ref
      let lvl = fromMaybe pos indent
      x <- if | pos <= ref -> incorrectIndent GT ref pos
              | pos == lvl -> p
              | otherwise -> incorrectIndent EQ lvl pos
      xs <- indentedItems ref lvl sc p
```
# Misc

>[!def] offending line
> The _offending line_ of some _code error_ is the line of source code that causes the error.

# References

https://serokell.io/blog/parser-combinators-in-haskell

http://jakewheat.github.io/intro_to_parsing

https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing


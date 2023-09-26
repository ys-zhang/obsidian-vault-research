#Haskell #meta-programming #DSL 

A `QuosiQuoter` is just a collection of parsers.

```haskell
data QuasiQuoter = QuasiQuoter
    { quoteExp :: String -> Q Exp  -- parser for expression
    , quotePat :: String -> Q Pat  -- parser for pattern
    , quoteType :: String -> Q Type
    , quoteDec :: String -> Q [Dec]
    }
```
The different fields are used when using the quasi-quoter in different places in your Haskell program: at a position where we expect a (Haskell) expression, a pattern (we will see an example of that later), a type or a declaration。

```haskell
{-# LANGUAGE QuasiQuotes #-}
qq = QuasiQuoter { .. }

-- not a quote but a splive
val = [qq| some dsl ... |] 
-- equiv to
val = $(quoteExp qq "some dsl ...")
```

>[!WARNING] 
>`[qq| xxx |]` is a splice, not a quote
# Convert Parser to QQ

suppose we have a parser 
```haskell
parser :: Parser AstNode
quote :: String -> Q Exp
```
to convert a parser to a quasi-quoter we need a function 
```haskell
dataToExpQ :: AstNode -> Q Exp
```
the function are usually implemented using [[Generic Programming in Haskell#SYB Scrap Your Boilerplate|SYB]]

>[!WARNING] Deprecate 
> `dataToExpQ` is deprecated, use `liftData` instead 

# Add anti-quotation

A common trick is to add new data constructors for each AST Node type.

```haskell
data Var = V String 
         | AV String  -- for antiquote
    deriving (Typeable, Data)

data Exp = Var Var 
         | Lam Var Exp
         | App Exp Exp 
         | AE String  -- for antiquote
    deriving (Typeable, Data)

antiVarE :: Var -> Maybe ExpQ
antiVarP :: Var -> Maybe PatQ
antiExpE :: Exp -> Maybe ExpQ
antiExpP :: Exp -> Maybe PatQ
```

# References

http://edsko.net/2013/05/09/brief-intro-to-quasi-quotation/
https://well-typed.com/blog/2014/10/quasi-quoting-dsls/

[haskell-src-meta: parse source to TemplateHaskell AST](https://hackage.haskell.org/package/haskell-src-meta)

see also [[Template Haskell]]

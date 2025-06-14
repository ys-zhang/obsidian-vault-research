# Concepts

1. **Reification**
    Reiﬁcation is Template Haskell’s way of allowing the programmer (_at compile time_) to query the state of the compiler’s internal (symbol) tables, asking questions such as “What is the line number in the source-ﬁle of the current position?”, or “What is the kind of this type constructor?” 
2. **splice**
    splicing means _evaluate_ some data represents an AST at compile, and insert the result at the call site. It is the inverse of (quasi) quote.
    - expression: `$[|e|] = e`
    - declaration: `$[d| decl |] = decl`
    - type: `$[t| typ |] = typ`
3. **static scope**
    Static scoping is also called _lexical scoping_. In this scoping, a variable always refers to its top-level environment. This is a property of the program text and is unrelated to the run-time call stack. _(TH is static scoped)_

>[!Example]
> In the following example, the occurrence of x in the RHS
> of g is indissoluably bound to the ﬁrst deﬁnition of x. 
> The fact that the call to g splices the code into a scope 
> where a different x is bound makes no difference.
> ```haskell
>    x :: Int
>    x = 3
>    
>    g :: Int -> ExpQ 
>    g y = [| x + y |]
> 
>    f :: a -> Int 
>    f x = $(g 4)
> ```


# The 3-Layers design

To summarise, in Template Haskell there are three “layers” to the representation of object-programs, in order of increasing convenience and decreasing power.


- The bottom layer has two parts. First, ordinary algebraic data types represent Haskell program fragments (Section 6.2). Second, the quotation monad, Q, encapsulates the notion of generating fresh names, as well as failure and input/output (Section 8).
- A library (`Language.Haskell.TH.Lib`) of _syntax-construction functions_, such as `tup` and `app`, lift the corresponding algebraic data type constructors, such as `Tup` and `App`, to the quotation-monad level, providing a convenient way to access the bottom layer (Section 6.3).
- The quasi-quote notation, introduced in Section 2, is most convenient but, as we have seen, there are important meta-programs that it cannot express. 


# The bottom layer: Syntax ADT


Defined in `Language.Haskell.TH.Syntax`. 

These ADTs usually require names. 
To make the generated code comply with _static scoping_

The quasi monad `Q` represent the action to generate names/identifies for AST.


```haskell 
class (Monad m, MonadFail m) => Quasi m where 
  -- | make a new name 
  qNewName :: String -> m Name
  -- | compile time reflection
  qReify :: Name -> m Info 
  qLocation :: m Loc
```


>[!Note] Name
> You can use `''` to acquire a name of some defined Haskell Type.
> For example, `''Int` . 


# The middle layer: syntax-construction functions


A lower-cased function (e.g. `app`) is the monadic version of its upper-cased data constructor (e.g. `App`)

```haskell
type ExpQ = Q Exp
app :: ExpQ -> ExpQ -> ExpQ   -- syntax-contruction function
App :: Exp -> Exp -> Exp      -- syntax ADT
```




# Other Materials

The sections _Names_, _Lifting Haskell Values to Expressions_ and _Typed Expression_ are worth to read.

<iframe src="https://markkarpov.com/tutorial/th.html"
        width="100%" height="600">
</iframe>






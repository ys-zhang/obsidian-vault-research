
#Haskell 

# Challenges 

Traditional _printf debugging_ technique does not work out in Haskell.
1. printing out Haskell variables requires `Show` instance, which may not be satisfied;
2. In stead of loops, recursive structure shrinks the input data, which makes printing a subset of the data;
3. printing evaluates the value required by the message to be printed, which changes program's semantics as Haskell is lazy.


# Toolbox


## `Debug.Trace`

> All these functions (`trace`, `traceShow`, `traceIO`) evaluate the _**message**_ completely before printing it; so if the message is not fully defined, none of it will be printed.

```haskell
trace :: Sting -> a -> a
traceShow :: Show a => a -> b -> b
traceShowId :: Show a => a -> a

-- | besides trace, prints a call stack if one is available 
--   the call stack is only available if the program was compiled 
--   with `-prof`; otherwise traceStack behaves exactly like trace.
-- Entries in the call stack correspond to SCC annotations, 
--   so it is a good idea to use `-fprof-auto` or `-fprof-auto-calls`
--   to add SCC annotations automatically.
traceStack :: String -> a -> a
```


## `Hood`

The idea of [Hood](https://hackage.haskell.org/package/hood) is to _log_ the reductions done to evaluate a Haskell expression; after the evaluation is complete these logged information is printed.

### Observing Data Structure

```haskell
observe :: Observable a  -- how to print logs
        => String        -- a tag for readability of printed msg
        -> a             
        -> a
```

>[!NOTE]
>`observe` does not forcing the value of its arguments 


### Observing Function

```haskell
instance (Observable a, Observable b) => Observable (a -> b)
```

the instance logs all evaluated _input_, _output_ pair.


## `GHCi`

Commands:
- `:b`, `:break`: create a breakpoint
  - `:break someFunction`
  - `:break Yase.Core.Concept.FieldKind 2 0`: break at module .. line 2 column 0, by default br will set to most recent loaded file
- `:show breaks`: list all break points
- `:print`: show var with value known up to current breakpoint, unlike the `show` function, this command will not force the queried variable.
  - `:force` will _fully_ force the variable
  - to force to WHNF, use the function `seq`, then `:print`
- `:continue`

### Stepping 

GHCi offers two variants of stepping. Use [`:step`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:step) to enable all the breakpoints in the program, and execute until the next breakpoint is reached. Use [`:steplocal`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:steplocal)to limit the set of enabled breakpoints to those in the current top level function. Similarly, use [`:stepmodule`](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:stepmodule) to single step only on breakpoints contained in the current module.


# References

https://wiki.haskell.org/Debugging

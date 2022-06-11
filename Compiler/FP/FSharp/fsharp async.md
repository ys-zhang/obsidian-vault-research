see [[concurrency#Concepts]]

```fsharp
async { expression }    // an async block
```

The type of the expression is `Async<'T>`, where `'T` is the type returned by the expression when the `return` keyword is used.

The general approach is to 
1. create `Async` objects that represent the computation or computations that you want to run asynchronously;
2. and then start these computations by using one of the *triggering function*s.

computation can be triggered/run on 
1. the current thread
2. some background thread
3. a .NET Framework `task` object


| Operation | Meaning                       | General Meaning                                                  |
| --------- | ----------------------------- | ---------------------------------------------------------------- |
| `let!`    | `await` binding, see[[#^let]] | bind to the result of computation expression                     |
| `do!`     | `await` for `unit` result     | calling a computation expression that returns a `unit`-like type |
| `return`  | wraps the result with a monad |                                                                  |
| `return!` | `await` and wrap              |                                                                  |
| `match!`  | extract from monad and match  |                                                                  |

The effect of `let!` is to enable execution to continue on other computations or threads as the computation is being performed. After the right side of the `let!` binding returns, the rest of the asynchronous workflow resumes execution. ^let

```fsharp
// let just stores the result as an asynchronous operation.
let (result1 : Async<byte[]>) = stream.AsyncRead(bufferSize)
// let! completes the asynchronous operation and returns the data.
let! (result2 : byte[])  = stream.AsyncRead(bufferSize)
```


#  Demand-driven concurrent model  ( Lazy concurrent model )

[Async Lazy - .NET Parallel Programming](https://devblogs.microsoft.com/pfxteam/asynclazyt/)

F# has `async` and `lazy` support for _demand-driven concurrent model_

> `Lazy<T>` is all about _caching a value and synchronizing multiple threads_ attempting to get at that cached value, whereas we have another type in the .NET Framework focused on representing an asynchronous operation and making its result available in the future: `Task<T>`



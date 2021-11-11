# Concepts

-   **Concurrency**; when multiple computations execute in overlapping time periods.
-   **Parallelism**; when multiple computations or several parts of a single computation run at exactly the same time.
-   **Asynchrony**; when one or more computations can execute separately from the main program flow.

> *TAKEAWAY*: asynchronous computations are independent of the main program flow.

# See
[Async Programming - F# | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/asynchronous-and-concurrent-programming/async)

[Asynchronous Workflows - F# | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/asynchronous-workflows)

[[Rust#Fearless Concurrency]]

# Declarative Concurrency

It is based on the fact that a dataflow variable can be bound to only one value. This gives the following two consequences:

1.  _What stays the same_: **The result of a program is the same whether or not it is concurrent**. Putting any part of the program in a thread does not change the result.
2.  _What is new_: **The result of a program can be calculated incrementally**. If the input to a concurrent program is given incrementally, then the program will calculate its output incrementally as well.

see [[fsharp async]]

#### declarative concurrent
A concurrent program is declarative if the following holds for all possible inputs. All executions with a given set of inputs have one of two results: 
1. they all do not terminate or 
2. they all eventually reach [[# partial termination]] and give results that are logically equivalent.

#### partial termination

The program never terminates. However, if the input stream stops growing, then the program will eventually stop executing too. This is an important insight. We say that the program does a partial termination.


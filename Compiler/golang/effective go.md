[Effective Go - The Go Programming Language (golang.org)](https://golang.org/doc/effective_go)

## Commentary

Comments do not need extra formatting such as banners of stars.

*Doc comments work best as complete sentences*, which allow a wide variety of automated presentations. *The first sentence should be a one-sentence summary that starts with the name being declared.*

Go's declaration syntax allows grouping of declarations. A single doc comment can introduce a group of related constants or variables.
*Grouping can also indicate relationships between items*, such as the fact that a set of variables is protected by a mutex.

```go
var (
    countLock   sync.Mutex
    inputCount  uint32
    outputCount uint32
    errorCount  uint32
)
```

## Names

*Packages are given lower case, single-word names*; there should be no need for `under_scores` or `mixedCaps`.

If you have a field called `owner` (lower case, unexported), the *getter method* should be called `Owner` (upper case, exported), not `GetOwner`.

 *interfaces are named* by the method name plus an `-er` suffix or similar modification to construct an agent noun.
 
 Finally, the convention in Go is to use `MixedCaps` or `mixedCaps` rather than underscores to write multiword names.
 
 
## Semicolons
 
 The rule is this. If the last token before a newline is an identifier (which includes words like `int` and `float64`), a basic literal such as a number or string constant, or one of the tokens
 
 
## Redeclaration and reassignment

```go
f, err := os.Open(name) // declare and assign `f` and `err`
d, err := f.Stat()      // declares `d` but only reassign `err`
```

looks as if it declares `d` and `err`. Notice, though, that `err` appears in both statements. This duplication is legal: `err` is declared by the first statement, but only _re-assigned_ in the second. This means that the call to `f.Stat` uses the existing `err` variable declared above, and just gives it a new value.

## Control flow

### switch

The expressions need not be constants or even integers, the cases are evaluated top to bottom until a match is found, and if the `switch` has no expression it switches on `true`.

 It's therefore possible—and idiomatic—to write an `if`-`else`-`if`-`else` chain as a `switch`.
 
 ```go
 func unhex(c byte) byte {
    switch {
    case '0' <= c && c <= '9':
        return c - '0'
    case 'a' <= c && c <= 'f':
        return c - 'a' + 10
    case 'A' <= c && c <= 'F':
        return c - 'A' + 10
    }
    return 0
}
```


#### break in switch

`break` statements can be used to terminate a `switch` early. Sometimes, though, it's necessary to break out of a surrounding loop, not the switch, and in Go that can be accomplished by *putting a label on the loop and "breaking" to that label.*

```go
Loop:
	for {
		switch {
		case ...:
			if validateOnly {
				break 
			}
			...
		case terminate:
			break Loop
		default:
			...
		}
	}
```

> Go only runs the selected case, not all the cases that follow. In effect, the `break` statement that is needed at the end of each case in those languages is provided automatically in Go.

#### Type switch

```go
var t interface{}
t = functionOfSomeType()
switch t := t.(type) {
default:
    fmt.Printf("unexpected type %T\n", t)     // %T prints whatever type t has
case bool:
    fmt.Printf("boolean %t\n", t)             // t has type bool
case int:
    fmt.Printf("integer %d\n", t)             // t has type int
case *bool:
    fmt.Printf("pointer to boolean %t\n", *t) // t has type *bool
case *int:
    fmt.Printf("pointer to integer %d\n", *t) // t has type *int
}
```

## Function 

### defer

> The arguments to the deferred function (which include the receiver if the function is a method) are evaluated when the _defer_ executes, not when the _call_ executes.

- Deferred functions are executed in LIFO order.

## Data allocation

Go has two allocation primitives, the built-in functions `new` and `make`. They do different things and apply to different types, which can be confusing, but the rules are simple.

### new

> `new(File)` and `&File{}` are equivalent.

`new` allocates memory, but unlike its namesakes in some other languages it does not _initialize_ the memory, it only _zeros_ it. 

That is, `new(T)` allocates zeroed storage for a new item of type `T` and returns its address, a value of type `*T`. In Go terminology, it returns a pointer to a newly allocated zero value of type `T`.

> Since the memory returned by `new` is zeroed, it's helpful to arrange when designing your data structures that the zero value of each type can be used without further initialization.

### make

The built-in function `make(T,` _args_`)` serves a purpose different from `new(T)`.

*It creates slices, maps, and channels only*, and it returns an _initialized_ (not _zeroed_) value of type `T` (not `*T`).

> The reason for the distinction is that these three types (slices, maps, and channels) represent, under the covers, references to data structures that must be initialized before use.

## Array and Slice
There are major differences between the ways arrays work in Go and C. In Go,

-   Arrays are values. Assigning one array to another copies all the elements.
-   In particular, if you pass an array to a function, it will receive a _copy_ of the array, not a pointer to it.
-   The size of an array is part of its type. The types `[10]int` and `[20]int` are distinct.

The length of a `slice` may be changed as long as it still fits within the limits of the underlying array; just assign it to a slice of itself. The _capacity_ of a `slice`, accessible by the built-in function `cap`, reports the maximum length the `slice` may assume. Here is a function to append data to a `slice`. If the data exceeds the capacity, the `slice` is reallocated.

## Print

```go
type T struct {
    a int
    b float64
    c string
}
t := &T{ 7, -2.35, "abc\tdef" }
fmt.Printf("%v\n", t)
fmt.Printf("%+v\n", t)
fmt.Printf("%#v\n", t)
fmt.Printf("%#v\n", timeZone)
fmt.Printf("%T\n", timeZone)
```
outputs:
```
&{7 -2.35 abc   def}
&{a:7 b:-2.35 c:abc     def}
&main.T{a:7, b:-2.35, c:"abc\tdef"}
map[string]int{"CST":-21600, "EST":-18000, "MST":-25200, "PST":-28800, "UTC":0}
map[string]int
```

## init function
Finally, each source file can define its own niladic `init` function to set up whatever state is required. (Actually each file can have multiple `init` functions.) And finally means finally: `init` is called after all the variable declarations in the package have evaluated their initializers, and those are evaluated only after all the imported packages have been initialized.

初始化流程
![[Pasted image 20210923231245.png]]

## Interface

Interfaces in Go provide a way to specify the behavior of an object: 
> if something can do _this_, then it can be used _here_.

A *type assertion* takes an interface value and extracts from it a value of the specified explicit type. `val, ok:=value.(typeName)`

## Embedding (sub-class)

>There's an important way in which embedding differs from subclassing. 
>When we embed a type, the methods of that type become methods of the outer type, but when they are invoked the receiver of the method is the inner type, not the outer one. (继承不是virtual)

If we need to refer to an embedded field directly, the type name of the field, ignoring the package qualifier, serves as a field name, as it did in the `Read` method of our `ReadWriter` struct.

Embedding types introduces the problem of name conflicts but the rules to resolve them are simple. 
- First, a field or method `X` hides any other item `X` in a more deeply nested part of the type.
- Second, if the same name appears at the same nesting level, it is usually an error; it would be erroneous to embed `log.Logger` if the `Job` struct contained another field or method called `Logger`. However, if the duplicate name is never mentioned in the program outside the type definition, it is OK.


## Concurrency
> Do not communicate by sharing memory; instead, share memory by communicating.
> Although Go's approach to concurrency originates in Hoare's [[Communicating Sequential Processes (CSP)]], it can also be seen as a type-safe generalization of Unix pipes.

> A goroutine has a simple model: it is a function executing concurrently with other goroutines in the same address space.


## Errors

```go
type error interface {
    Error() string
}
```

> `Error` is returned to indicate the caller. 
> Callers that care about the precise error details can use a type switch or a type assertion to look for specific errors and extract details.

When feasible, error strings should identify their origin, such as by having a prefix naming the operation or package that generated the error.

## panic

> But what if the error is unrecoverable? Sometimes the program simply cannot continue.

When `panic` is called, it immediately stops execution of the current function and begins unwinding the stack of the goroutine, running any deferred functions along the way. If that unwinding reaches the top of the goroutine's stack, the program dies.

A call to recover stops the unwinding and returns the argument passed to panic. Because the only code that runs while unwinding is inside deferred functions, recover is only useful inside deferred functions.



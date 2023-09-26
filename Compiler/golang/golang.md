# Some books

## The Go Programming Language


- official:  [The Go Programming Language (gopl.io)](http://www.gopl.io/)
- repo: [gopl-zh/gopl-zh.github.com: Go语言圣经中文版(只接收PR, Issue请提交到golang-china/gopl-zh)](https://github.com/gopl-zh/gopl-zh.github.com)
- online read: [前言 · Go语言圣经 (itsfun.top)](https://book.itsfun.top/gopl-zh/)

## Advanced go programming

- repo: [repo 《Go语言高级编程》开源图书，涵盖CGO、Go汇编语言、RPC实现、Protobuf插件实现、Web框架实现、分布式系统等高阶主题(完稿)](https://github.com/chai2010/advanced-go-programming-book)
- [online](https://chai2010.cn/advanced-go-programming-book/)

## Go语言设计与实现
[Go 语言设计与实现](https://draveness.me/golang/)


## Go 极客兔兔
- [Go 语言高性能编程 | 极客兔兔 (geektutu.com)](https://geektutu.com/post/high-performance-go.html)
- [geektutu/high-performance-go: high performance coding with golang（Go 语言高性能编程，Go 语言陷阱，Gotchas，Traps） (github.com)](https://github.com/geektutu/high-performance-go)
- [7天用Go从零实现Web框架Gee教程 | 极客兔兔 (geektutu.com)](https://geektutu.com/post/gee.html)

# Sections

- database: [[golang database programming.excalidraw]]
- compiler: [[go-compiler]]


# Packages

- [golang/groupcache: groupcache is a caching and cache-filling library, intended as a replacement for memcached in many cases. (github.com)](https://github.com/golang/groupcache): go version of `memcached`

# Caveats

```go
// this is wrong!!!
for _, input := range mkIterable() {
  go function() {
      // closure only captures the name/ref 
      // not the value
      // i.e. as is closures without move 
      //      in Rust
      doSomeActionOn(input)
  }
}

// do this 
for _, input := range mkIterable() {
  go function(input) {
    doSomeActionOn(input)
  }(input) // works as Go use pass by value
}
```

- Caveat: An Interface Containing a Nil Pointer Is Non-Nil, i.e., A _nil interface value_, which contains no value at all, is not the same as _an interface value containing a pointer that happens to be nil_.

```go
const debug = false

// If out is non-nil, output will be written to it.
func f(out io.Writer) {
  // ...do something...
  if out != nil {
    out.Write([]byte("done!\n"))
  }
}

func wrongVersion() {
  var buf *bytes.Buffer
  if debug {
    buf = new(bytes.Buffer) // enable collection of output
  }
  f(buf) // NOTE: subtly incorrect!
  if debug {
    // ...use buf...
  }
}

func rightVersion() {
  var buf io.Writer
  if debug {
    buf = new(bytes.Buffer) // enable collection of output
  }
  f(buf) // NOTE: subtly incorrect!
  if debug {
    // ...use buf...
  }
}


```

when `debug = false`, called from `wrongVersion` in `f`,  `out` is a non-nil interface containing a nil pointer value


>[!Error] take away
> Be careful when testing interface value to `nil`


# Language References

- The case of the first letter of a name determines its _visibility_ across package boundaries. If the name begins with an _upper-case_ letter, it is exported, which means that it is visible and accessible outside of its own package and may b  referred to by other parts of the program.
- Stylistically, Go programmers use ‘‘camel case’’ when forming names by combining words; that is, interior capital letters are prefer red over interior underscores.
- Variable declaration, one of `type` and `expr` can be omitted. 
  ```go
  var name type = expr
  name := expr
  type Kind int 
  const (
    KdLit  = 1
    KdExpr = 2
    KdDecl = 3
  ) 
  ```
- If a variable is declared `var x int`, the expression `&x` (‘‘address of x’’) yields a pointer to an integer variable, that is, a value of type `*int`, which is pronounced ‘‘pointer to `int`.’’ If this value is called `p`, we say ‘‘`p` points to `x`,’’ or equivalently ‘‘`p` contains the address of `x`.’’ The variable to which `p` points is written `*p`. The expression `*p` yields the value of that variable, an int, but since `*p` denotes a variable, it may also appear on the left-hand side of an assignment
- Because a pointer contains the address of a variable, passing a pointer argument to a function makes it possible for the function to update the variable that was indirectly passed.
- Another way to create a variable is to use the built-in function new. The expression `new(T)` creates an unnamed variable of type `T`, initialises it to the zero value of `T`, and returns its address, which is a value of type `*T`.
- The _lifetime_ of a variable is the interval of time during which it exists as the prog ram executes. The lifetime of a package-level variable is the entire execution of the program.
- (_escape analysis_) A compiler may choose to allocate local variables on the heap or on the stack but, perhaps surprisingly, this choice is not determined by whether `var` or `new` was used to declare the variable.
- (_type declaration_) 
  ```go
  type Name UnderlyingType
  ```
  For every type `T`, there is a corresponding conversion operation `T(x)` that converts the value `x` to type `T`. A conversion from one type to another is allowed if both have the same underlying type, or if both are unnamed pointer types that point to variables of the same underlying type; these conversions change the type but not the representation of the value.
- (_basic types_)
   1. `int` and `uint` are machine dependent
   2. `rune` is a synonym for `int32` and conventionally indicates its value represents a unicode point.
   3. there is an unsigned integer type `uintptr`, whose width is not specified but is sufficient to hold all the bits of a pointer value.
   4. Go provides two sizes of floating-point numbers, `float32` and `float64`.
   5. A `string` is an immutable sequence of bytes.
- An `array` is a fixed-length sequence of zero or more elements of a particular type. `array`s are passed by value.
- Slices represent variable-length sequences whose elements al l have t he same type. A slice type is written `[]T`, where the elements have type `T`; it looks like an array type without a size. Internally its just a struct contains a length, capability and a pointer to the underlying storage. The slice itself is passed by value.
- (_function stack_) Typical Go implementation uses variable-size stacks whose limit is on the order of GB
- (_function_) functions/closures are _reference_ types and not comparable.
- (_variadic function_)
    ```go
    // type of vals is []int
    func sum(vals ...int) int {
      var xs []int = vals
      ...
    }
    ```
  - (_defer_) the function deferred and the arguments are evaluated when the defer statement is executed, i.e. at the place of the defer statement, but the call is deferred until the container function is finished. 
- (_error handling_)
    - A function for which failure is an expected behaviour returns an additional result, conventionally the last one.
    - the `error` in Go is an interface.
    - (_panic and recover_) Go has an _exception_ mechanism, but its reserved for reporting truely unexpected errors that indicate a bug, not some routine error that a robust program should be built to expect.
        ```go
        func f() {
          defer function() error {
            if p := recover(); p != nil {
              error(p)
            }
          }()
          // some thing may panic here
          ...
        }
        ```
    - `fmt.Errorf` formats a error message and returns an error.
    - message should not be capitalised and should not contain newline.
    - In general the call of function `f(x)` is responsible for reporting the _attempted operation_ and the _argument value_ as they relate to the context of the error. 
- (_interface satisfaction_) a struct type `T` satisfies some interface iff. the required method is implemented for the type `T` directly, i.e., the receiver must be type `T`

    ```go
  type IntSet struct { /* ... */ }
  func (*IntSet) String() string
  // compile error: String requires *IntSet receiver
  var _ = IntSet{}.String() 
  
  var s IntSet
  var _ = s.String() // OK: s is a variable and &s has a String method
  
  var _ fmt.Stringer = &s // OK
  var _ fmt.Stringer = s  // compile error: IntSet lacks String method
    ```

- A concrete type may satisfy many unrelated interfaces.
- (_dynamic dispatch_) interface method dispatching uses [vtable](https://groups.google.com/g/golang-nuts/c/osyRcx_hcH8)
- (_type assertion_) A type assertion is an operation applied to an _interface value_. The semantics of `interfaceValue.(SomeType)` is 

  ```go
var w io.Writer
w = os.Stdout
// success: f == os.Stdout
f := w.(*os.File) 
// panic: interface holds *os.File, not *bytes.Buffer
c := w.(*bytes.Buffer) 
// ok == false
c, ok := w.(io.ReadWriter) 
  ```
  
  - `SomeType` is a concrete type, the type assertion statement checks whether the value's concrete type is exactly the one provided
  - `SomeType` is an interface type, then the statement checks if the value's concrete type satisfies the provided interface.
- (_type switch_)
    ```go
    switch x.(type) {
      case nil:       // ...
      case int, uint: // ...
      case bool:
      case string:
      default:
    }
    ```


# OOP

## Method
In a realistic program, convention dictates that _if any method of Point has a pointer receiver, then all methods of Point should have a pointer receiver, even ones that don’t strictly need it_.

Furthermore, to avoid ambiguities, method declarations are not permitted on named types that are themselves pointer types.

The rule about pointers vs. values for receivers is that _value methods can be invoked on pointers and values, but pointer methods can only be invoked on pointers_. There is a handy exception, though. _When the value is addressable, the language takes care of the common case of invoking a pointer method on a value by inserting the address operator automatically._ In our example, the variable `b` is addressable, so we can call its `Write` method with just `b.Write`. The compiler will rewrite that to `(&b).Write` for us.

>[!Concept] Addressable
>A type is non-addressable if you cannot get the address of value of that type.
>This concept has some connection with rust's `Pin` type and `Unpin` auto trait, and `move` , `memcpy`. 
>In short, Addressable types are types that are pinned (guaranteed to have const address within its lifetime)
>
>Some non-addressable types including: 
> > The concrete value stored in an interface is not addressable, in the same way that a `map` element is not addressable.
> 
> See also https://www.sobyte.net/post/2022-01/not-addressable-in-golang/

## Struct Embedding

- Struct can have multiple _embeddings_
```go

type Point struct { X, Y float64 }

func (p *Point) Norm() float64 {
  math.Sqrt(p.X^2 + p.Y^2)
}

// all receivers (method or field) accept a Point can 
// accept a ColoredPoint
type ColoredPoint struct {
  Point // embed a Point struct
  Color color.RGBA
}

func embedDemo() {
  cpt := new(ColoredPoint)
  x := cpt.X // accessing field of struct Point
  norm := cpt.Norm()
}

```

## Interface Embedding

```go
type Writer interface {
  Write(p []byte) (n int, err error)
}

type Reader interface {
  Read(p []byte) (n int err error)
}

type Close interface {
  Close() error 
}

type ReadWriter interface {
  Reader
  Writer
}

type ReadWriteCloser interface {
  Reader
  Writer
  Closer
}
```

# Modules and Packages


## Package and Import

**Packages** provide encapsulation by controlling which names are visible or exported outside the package.

(_import path_) Go's language specification does not give import path any semantic. However, the `go` cli does. _Conventionally, packages names lives in the last segment of the import path_; furthermore, import paths shall be _globally unique_

```go
package main

import (
  "crypto/rand",
  mrand "math/rand"  // renaming import
  "database/mysql"
  // blank import
  // for side effect defined in `init` functions only
  _ "github.com/go-sql-driver/mysql" // enable mysql
  _ "github.com/lib/pq"   // enable postgre      
)
```

>[!note] Package Name, Import Path and File Name
> 1. _package name_ == `dirname` (i.e., directory name), except source file defining test code which has package name `dirname_test`
> A succinct example can be 
>   ```
>   | math    <------ root dir    
>   | | rand    <------ sub dir   
>   | | | rand.go    <------ package math/rand
>   | | | exp.go    <------ package math/rand
>   | | abs.go    <------ package math
>   | | floor.go        <------ package math
>   ``` 

_Single-type packages_, such as `html/template` and `math/rand` exposes one principal data type with its methods and often a `New` function to create instances


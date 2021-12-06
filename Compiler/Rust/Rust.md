#Rust

[Introduction - The Rust Programming Language (rust-lang.org)](https://doc.rust-lang.org/book/ch00-00-introduction.html)

[A Rust game development talk](https://kyren.github.io/2018/09/14/rustconf-talk.html)  


[AFLplusplus/LibAFL: Advanced Fuzzing Library](https://github.com/AFLplusplus/LibAFL)

[Rust Magazine 中文月刊](https://rustmagazine.github.io/rust_magazine_2021/index.html)

# Components & Concepts

- [[Rust Cmd Tools]]: dependency manager and build tool;
- `Rustfmt` ensures a consistent coding style across developers.
- _The Rust Language Server_ powers IDE integration for code completion and inline error messages.
- `rustc` the compiler
- _crate_: In Rust, packages of code are referred to as _crates_.
  


# Language

## Variable
```rust
// this is a comment

// variable binding
let plannets = 9;                 // immutable binding
let mut mylove = String::new();   // mutable binding
const MAX_POINTS: u32 = 100_000;  // a constant, must be type annotated

// some how pluto get kicked out
// this shadows prev defination
let plannets = 8;      

let x = (let y = 1);  // this is an error, statement do not have value
let mut x = 0;
let mut y = 0;

x = y = 1; // however asignment is an expression and thus have value

```


### Data Types

- [[#Reference]]
- [[#Slice]]

#### scalar type
- integer: (signed)`i8-i128`, (unsigned)`u8-u128`, (arch dependent) `isize`, `usize`, Signed numbers are stored using [[two’s complement]]. Default integer type is `i32`.
- floats: `f32`, `f64` (default)
- boolean: `bool` with value `true/false`
- char: `char` using single quote `'`, _occupies 4 bytes._

#### compound type
```rust
// Tuple
let tup: (i32, f64, u8) = (500, 6.4, 1);  // tuple
let (x, y, z) = tup;                      // unpack tuple
// same as
let x = tup.0; let y = tup.1; let z = tup.2; // tuple indexing


// Array
let a: [i32; 5] = [1, 2, 3, 4, 5];   // i32[5]
let first = a[0]; let second = a[1]; // array indexing
let b: [char; 100] = ['M'; 100];
```

##### Struct
```rust

struct User {
  username: String,
  email: String,
  sign_in_count: u64,
  active: bool,
}

let user1 = User {
    email: String::from("someone@example.com"),
    username: String::from("someusername123"),
    active: true,
    sign_in_count: 1,
};
// struct update syntax
let user2 = User {
    email: String::from("another@example.com"),
    username: String::from("anotherusername567"),
    ..user1
};

// tuple structs
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);


// method

#[derive(Debug)]   // deriving the Debug trait
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    // struct introduce a namespace
  
    // define methods
    // can be called by rect.area()
    fn area(&self) -> u32 {
        self.width * self.height
    }
    // assoiciate function
    // This function is namespaced by the struct
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }
}
```

##### Enumeration (the algebraic data type) 

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}
// can define methods on enums
impl Message { 
    fn call(&self) {
      // method body would be defined here 
    } 
}
let msg = Message::Write(String::from("jerrie"));
msg.call();

enum Option<T> { 
  None, 
  Some(T), 
}
```

###### Pattern Matching

```rust
enum UsState { Alabama, Alaska, }

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
    Illegal,
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
          println!("State quarter from {:?}!", state);
          25
        },
        _ => 0,  // Illegal coin values nothing
    }
}

// if-let binding
let mut count = 0;
if let Coin::Quarter(state) = coin {
    println!("State quarter from {:?}!", state);
} else {
    count += 1;
}

```



## Function
```rust
fn main() {
    add(5, 6);  // function call
}


fn add(x: i32, y: i32) -> i32 {  // signiture
  return x + y;  
}

fn add_implicit_return(x: i32, y: i32) -> i32 {
  // note these is no `;` 
  //   as `;` indicates its a statement 
  //   and statements has no value
  x + y
}
```

## Control flow

```rust
// condition value must be a bool
if number < 5 {  
  println!("condition was true");
} else {
  println!("condition was false");
}

let number = if condition { 5 } else { 6 };

// an infinite loop
loop { println!("again!"); }

// return a value in loop by break the value
let result = loop { 
  counter += 1; 
  if counter == 10 { 
    break counter * 2; 
  } 
};

// a while loop
while number != 0 {
  println!("{}!", number);
  number -= 1;
}

// for ... in ... loop
let a = [10, 20, 30, 40, 50];
for element in a.iter() {
    println!("the value is: {}", element);
}
for number in (1..4).rev() {
  println!("{}!", number);
}
```


## Functional Programming

```rust
let v: Vec<i32> = (1..)
  .filter(|x| x % 2 == 0)
  .take(5)
  .collect();


let fruit_kilo: u32 = basket.values().sum::<u32>();
```


### Iterator

> In Rust, iterators are _lazy_, meaning they have no effect until you call methods that consume the iterator to use it up.

```rust
// The Iterator trait and next method
pub trait Iterator {
  type Item;    // define an associate type
  fn next(&mut self) -> Option<Self::Item>;
  fn sum<S>(&mut self) -> S 
    where S: Sum<Self::Item>;
}


let v1 = vec![1, 2, 3];
let v1_iter = v1.iter();  // have no effect
// use iter to iterate a vector
// the for loop takes the ownership of iter
//   and makes it mutable
for val in v1_iter {
  println!("Got: {}", val);
}

// create an iterator that takes ownership
let v1_iter2 = v1.to_iter(); 

let mut v2 = vec![1, 2, 3];
// iterate over mutable vector
let mut v2_iter = v2.iter_mut();

```

[Iterator in core::iter - Rust (rust-lang.org)](https://doc.rust-lang.org/core/iter/trait.Iterator.html)

Methods that call `next` are called _consuming adaptors_, because calling them uses up the iterator.

### Closure / lambda function

Unlike functions, closures can capture values from the scope in which they’re defined.

```rust
let expensive_closure = |num| {
    println!("calculating slowly...");
    thread::sleep(Duration::from_secs(2));
    num
};

fn  add_one_v1   (x: u32) -> u32 { x + 1 }
let add_one_v2 = |x: u32| -> u32 { x + 1 };
let add_one_v3 = |x|             { x + 1 };
let add_one_v4 = |x|               x + 1  ;
```

The `Fn` traits are provided by the standard library. All closures implement at least one of the traits: `Fn`, `FnMut`, or `FnOnce`.

-   `FnOnce` consumes the variables it captures from its enclosing scope, known as the closure’s _environment_. To consume the captured variables, the closure must take ownership of these variables and move them into the closure when it is defined. The `Once` part of the name represents the fact that the closure can’t take ownership of the same variables more than once, so it can be called only once.
-   `FnMut` can change the environment because it mutably borrows values.
-   `Fn` borrows values from the environment immutably.

When a closure captures a value from its environment, it uses memory to store the values for use in the closure body.

The `move` keyword before the parameter list force the closure to take ownership of the values it uses in the environment

```rust
fn main() {
  let x = vec![1, 2, 3];
  // move x into the closure
  let equal_to_x = move |z| z == x;
  // panic
  println!("Can't use x here: {:?}", x); 
  let y = vec![1, 2, 3];
  assert!(equal_to_x(y));
}
```


### Recursive Type

see [[#Recursive Type using Box]]


### Pattern Matching

A pattern consists of some combination of the following:

-   Literals
-   Destructured arrays, enums, structs, or tuples
-   Variables
-   Wildcards
-   Placeholders

```rust
match VALUE {
  PATTERN => EXPRESSION,
  PATTERN => EXPRESSION,
  PATTERN => EXPRESSION,
}

let PATTERN = EXPRESSION;

if let PATTERN(var) = EXPRESSION { 
  // do sth with var
} else {
  // ...
}

match value {
  x if x < 0 => Err(CreationError::Negative),
  x if x == 0 => Err(CreationError::Zero),
  x => Ok(PositiveNonzeroInteger(x as u64))
}

while let Some(top) = stack.pop() { 
  println!("{}", top); 
}

for (index, value) in v.iter().enumerate() { 
  println!("{} is at index {}", value, index); 
}

// function parameters can also be patterns
fn print_coordinates(&(x, y): &(i32, i32)) { 
  println!("Current location: ({}, {})", x, y); 
}
```


Patterns come in two forms: _refutable_ and _irrefutable_.
- Patterns that will match for any possible value passed are _irrefutable_.
- Patterns that can fail to match for some possible value are _refutable_.

Function parameters, `let` statements, and `for` loops can only accept irrefutable patterns, because the program cannot do anything meaningful when values don’t match.

```rust
PATTERN = PATTERN | PATTERN
RANGE_PATTERN = PATTERN ..= PATTERN

// destruct struct
let p = Point { x: 0, y: 7 }; 
let Point { x: a, y: b } = p;
let Point { x: a, .. } = p; // ignore rest using ..
```
The _at_ operator (`@`) lets us create a variable that holds a value at the same time we’re testing that value to see whether it matches a pattern.

## Unsafe

```rust
unsafe {
  //unsafe code
}
```

_unsafe superpowers_:
-   Dereference a raw pointer
-   Call an unsafe function or method
-   Access or modify a mutable static variable
-   Implement an unsafe trait
-   Access fields of `union`s

>  `unsafe` doesn’t turn off the borrow checker or disable any other of Rust’s safety checks. The `unsafe` keyword only gives you access to these five features that are then not checked by the compiler for memory safety.

### Raw pointer
Unsafe Rust has two new types called _raw pointers_ that are similar to references. As with references, raw pointers can be _immutable_ or _mutable_ and are written as `*const T` and `*mut T`, respectively.

Different from references and smart pointers, raw pointers:

-   Are allowed to _ignore the borrowing rules_ by having both immutable and mutable pointers or multiple mutable pointers to the same location
-   Are _not guaranteed_ to point to _valid memory_
-   Are allowed to be _null_
-   Don’t implement any automatic cleanup

```rust
let mut num = 5;
// we can use raw pointer in safe mode
// but can not dereference them
let r1 = &num as *const i32;
let r2 = &mut num as *mut i32;
```

### Unsafe function

```rust
unsafe fn dangerous() {
  // in unsafe mode
} 

unsafe { 
  // in unsafe mode
  dangerous(); 
}
```

### extern functions

Rust has a keyword, `extern`, that facilitates the creation and use of a _Foreign Function Interface (FFI)_.

```rust
extern "C" {  // "C": application binary interface (ABI)
    fn abs(input: i32) -> i32;
}

fn main() {
    unsafe {
        println!("Absolute value of -3 according to C: {}", abs(-3));
    }
}

```

> We can also use `extern` to create an interface that allows other languages to call Rust functions.

```rust

// `call_from_c` function accessible from C code,
//  after it’s compiled to a shared library and linked from C.
#[no_mangle]
pub extern "C" fn call_from_c() {
    println!("Just called a Rust function from C!");
}

```

### Global/static variable
Static variables can be mutable. Accessing and modifying mutable static variables is _unsafe_.


### Unsafe trait
A _trait_ is _unsafe_ when at least one of its methods has some invariant that the compiler can’t verify.

## OOP

### Encapsulate
 The `pub` keyword to decide which modules, types, functions, and methods in our code should be public, and by _default everything else is private_.

### Inheritance
> There is no way to define a struct that inherits the parent struct’s fields and method implementations.

You can share Rust code using _default trait method_ implementations instead.

In Rust, we refrain from calling `structs` and `enums` “objects” to distinguish them from other languages’ objects. 

In a `struct` or `enum`, the data in the struct fields and the behavior in `impl` blocks are _separated_, whereas in other languages, the data and behavior combined into one concept is often labeled an object. 

However, trait objects _are_ more like objects in other languages in the sense that _they combine data and behavior_. But trait objects differ from traditional objects in that _we can’t add data to a trait object_.

> We create a trait object by specifying some sort of pointer, such as a `&` reference or a `Box<T>` smart pointer, then the `dyn` keyword, and then specifying the relevant trait.

> A generic type parameter can only be substituted with one concrete type at a time, whereas trait objects allow for multiple concrete types to fill in for the trait object at runtime.

> When we use trait objects, Rust must use dynamic dispatch.


A trait is object safe if all the methods defined in the trait have the following properties:
-   The return type isn’t `Self`.
-   There are no generic type parameters.

### Trait

####  Associated type

_Associated types_ connect a _type placeholder_ with a _trait_ such that the trait method definitions can use these placeholder types in their signatures. 

That way, we can define a trait that uses some types without needing to know exactly what those types are until the trait is implemented.

The _difference is that when using generics_, as in Listing 19-13, we must annotate the types in each implementation; because we can also implement `Iterator<String> for Counter` or any other type, we could have multiple implementations of `Iterator` for `Counter`. In other words, _when a trait has a generic parameter, it can be implemented for a type multiple times_, changing the concrete types of the generic type parameters each time. When we use the `next` method on `Counter`, we would have to provide type annotations to indicate which implementation of `Iterator` we want to use.

#### Default Generic type parameter

When we use generic type parameters, we can specify a _default concrete type_ for the generic type. This eliminates the need for implementors of the trait to specify a concrete type if the default type works.

```rust


trait Add<Rhs=Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}


```

#### `supertrait`

%%todo%%

### Macro

> Macros can take a variable number of parameters but functions can not.

> You must define macros or bring them into scope _before_ you call them in a file, as opposed to functions you can define anywhere and call anywhere.

#### Declarative macro

_Declarative macros_ allow you to write something similar to a Rust `match` expression.

> Macros compare a value to patterns that are associated with particular _code_: in this situation, the value is the_ literal Rust source code_ passed to the macro; the _patterns are compared with the structure of that source code_; and the code associated with each pattern, when matched, replaces the code passed to the macro.

```rust
#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

The `#[macro_export]` annotation indicates that this macro should be made available whenever the crate in which the macro is defined is brought into scope. Without this annotation, the macro can’t be brought into scope.


#### Procedural macro

> The second form of macros is _procedural macros_, which act more like functions (and are a type of procedure). 

> Procedural macros _accept some code as an input, operate on that code, and produce some code as an output rather_ than matching against patterns and replacing the code with other code as declarative macros do.

The three kinds of procedural macros (_custom derive_, _attribute-like_, and _function-like_) all work in a similar fashion.

The function that defines a procedural macro takes a `TokenStream` as an input and produces a `TokenStream` as an output.

```rust
use proc_macro;

#[some_attribute] // placeholder for using a specific macro
pub fn some_macro(input: TokenStream) -> TokenStream {
}
```

## Packaging

A package can contain _multiple binary crates_ and _optionally one library crate_.

### Some Concepts
-   **Packages:** A Cargo feature that lets you build, test, and share crates

-   **Crates:** A tree of modules that produces a library or executable. A Crate contains a Scope
    -   **Crate root** A source file that the Rust compiler starts from and makes up the root module of your crate
    - **src/main.rs** Cargo follows a convention that _src/main.rs_ is the crate root of _a binary crate_ with the same name as the package.
    - **src/lib.rs** The package contains _a library crate_ with the same name as the package, and _src/lib.rs_ is its crate root.
  
-   **Modules** and **use:** Let you control the organization, scope, and privacy of paths
    -   _Modules_ let us organize code within a crate into groups for readability and easy reuse.
    -   Modules also control the _privacy_ of items

-   **Paths:** A way of naming an item, such as a struct, function, or module
    - `use`  keyword that brings a **Path** into scope;
    - the `pub` keyword to make items public.
    - A path can take two forms:
        1.   An _absolute path_ starts from a crate root by using a crate name or a literal `crate`.
        2.   A _relative path_ starts from the current module and uses `self`, `super`, or an identifier in the current module.

> _Modules_ are like folders in a file system and _Paths_ are like paths of folders. We can use _Paths_ to refer to / find _Modules_ 


```rust

// load the contents of the module from another file with the same name as the module.
mod some_module; 
```

### Privacy
The way privacy works in Rust is that all items (functions, methods, structs, enums, modules, and constants) are private by default.

Items in a parent module can’t use the private items inside child modules, but items in child modules can use the items in their ancestor modules.

#### The `pub` keyword

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

pub fn eat_at_restaurant() {
    // Absolute path
    crate::front_of_house::hosting::add_to_waitlist();

    // Relative path
    front_of_house::hosting::add_to_waitlist();
}
```


- If we use `pub` before a struct definition, we make the struct public, but the struct’s fields will still be private.
- if we make an enum public, all of its variants are then public.

#### The `use` keyword

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

Adding `use` and a path in a scope is similar to creating a symbolic link in the filesystem. By adding `use crate::front_of_house::hosting` in the crate root, `hosting` is now a valid name in that scope.

`mod front_of_house;` tells Rust to load the contents of the module from another file with _the same name as the module_.

## Error handling

Rust groups errors into two major categories: _recoverable_ and _unrecoverable_ errors. It has the type `Result<T, E>` for _recoverable errors_ and the `panic!` macro that stops execution when the program encounters an _unrecoverable error_.


```rust
enum Result<T, E> { 
  Ok(T), 
  Err(E),
}

let val: i32 = Result::Ok(1).unwrap();
let val: i32 = Result::Err(err).unwrap(); // panic!
```


## Generics

```rust
struct Point<T, U> { 
  x: T, 
  y: U, 
}
impl<T,U> Point<T,U> { 
  fn x(&self) -> &T { 
    &self.x 
  }
  fn mixup<V, W>(self, other: Point<V, W>) -> Point<T, W> {
    Point {
      x: self.x,
      y: other.y,
    }
  }
}


// define trait, like class in haskell
pub trait Summary { 
  fn summarize(&self) -> String;
  fn default_summarize(&self) -> String { 
    String::from("(Read more...)") 
  }
}

impl<T: Display, U: Display> Summary for Point<T, U> { 
  fn summarize(&self) -> String { 
    format!("(x: {} y: {})", 
            self.T,
            self.U) 
  } 
}

// trait bound
pub fn notify<T: Summary>(item: &T) { 
  println!("Breaking news! {}", item.summarize()); 
}

pub fn notify(item: &(impl Summary + Display)) {}
pub fn notify<T: Summary + Display>(item: &T) {}
fn returns_summarizable() -> impl Summary {}
fn some_function<T, U>(t: &T, u: &U) -> i32 
  where T: Display + Clone, U: Clone + Debug 
{
  // body
}
```

## Lifetime 
see [[#Lifetime of references]]


#  Fearless Concurrency

> By leveraging ownership and type checking, many concurrency errors are _compile-time errors_ in Rust rather than runtime errors.


## Threads

> The green-threading M:N model requires a _larger language runtime_ to manage threads. As such, the Rust standard library _only provides an implementation of 1:1 threading_.

```rust
use std::thread;
fn main() {
  let handle = thread::spawn(|| {
    // calculation
  });
  
  handle.join().unwrap();
}
```

> The `move` closure is often used alongside `thread::spawn` because it allows you to use data from one thread in another thread. Thread can only access variable it owns.

## Message passing

> “Do not communicate by sharing memory; instead, share memory by communicating.”   -- From Golang.

A channel is said to be _closed_ if _either_ the transmitter or receiver half is _dropped_.

- `std::sync::mpsc`: multiple producer single consumer.
- `let (tx, rx) = mpsc::channel();` The function returns a tuple, the _first element_ of which is the _sending end_ and the _second element_ is the _receiving end_.
- `let data: Result<T, E> = rx.recv();` blocking
- `let data: Result<T, E> = rx.try_recv();` non-blocking
- `let tx1 = tx.clone();` clone to create another sender.



## Shared state

- `std::sync::Mutex` mutex

```rust
use std::sync::{Arc, Mutex};
use std::thread;

// a mutex protects i32
let counter: Mutex<i32> = Arc::new(Mutex::new(5));  
let mut handles = vec![];  // thread handles

for _ in 1..10 {
  let counter = Arc::clone(&counter);
  
  let handle = thread::spawn(move || {
    let mut num = counter.lock().unwrap();
    *num += 1;
  });
  handles.push(handle);
}
```
Similarities Between `RefCell<T>/Rc<T>` and `Mutex<T>/Arc<T>`


## Marker traits `Send` & `Sync`

The `Send` marker trait indicates that ownership of values of the type implementing `Send` can be transferred between threads.

Any type composed entirely of `Send` types is automatically marked as `Send` as well. Almost all primitive types are `Send`, aside from raw pointers.

The `Sync` marker trait indicates that it is safe for the type implementing `Sync` to be referenced from multiple threads. In other words, any type `T` is `Sync` if `&T` (an immutable reference to `T`) is `Send`, meaning the reference can be sent safely to another thread.

Because types that are made up of `Send` and `Sync` traits are automatically also `Send` and `Sync`, we don’t have to implement those traits manually. As marker traits, they don’t even have any methods to implement. They’re just useful for enforcing invariants related to concurrency.

Manually implementing these traits involves implementing _unsafe Rust code_.


# Prelude & Standard Library


## Memory related traits & structs

### `Deref` & `DerefMut` trait

Implementing the `Deref`/`DerefMut` trait allows you to customize the behavior of the _dereference operator_, `*`  on immutable/mutable references.

```rust
struct MyBox<T> (T);

impl<T> MyBox<T> {
  fn new(x: T) -> MyBox<T> {
    MyBox(x)
  } 
}

impl<T> Deref for MyBox<T> {
  // associated type for the `Deref` trait to use
  type Target = T;   
  
  fn deref(&self) -> Self::Target {
    &self.0
  }
  
}

fn main() {
  let x = 5;
  let y = MyBox::new(x);  // reference a value on stack
  assert_eq!(5, *y);      // *(y.deref())
}
```


### `Drop` trait

`Drop` trait defines what happens when a value is _about to go out of scope_.

The `Drop` trait requires you to implement one method named `drop` that takes a mutable reference to `self`.

Variables are dropped in the _reverse order_ of their creation.

Dropping a Value Early with `std::mem::drop`.


###   `Box<T>`

_Boxes_ allow you to store data on the heap rather than the stack. What remains on the stack is the pointer to the heap data.

Boxes don’t have performance overhead, other than storing their data on the heap instead of on the stack. 

**Typical scenario**:
-   When you have a type whose _size can’t be known at compile time_ and you want to use a value of that type in a context that requires an exact size
-   When you have a large amount of data and you want to _transfer ownership but ensure the data won’t be copied_ when you do so
-  [[#trait object]] When you want to own a value and you care only that it’s a type that _implements a particular trait_ rather than being of a specific type

#### Recursive Type using Box

> At compile time, Rust needs to know how much space a type takes up. However, boxes have a known size, so by inserting a box in a recursive type definition, you can have recursive types.

Cons List:

```rust
// Single the memory of a List<T> object is unknow at compile
//   time. Thus it is impossible to alloc memory for such
//   an object.
enum List<T> {
  Cons(T, List<T>),
  Nil,
}

// We need to add an indirection. The following works since 
//  the size of Box<T> is independent of T.
enum List<T> {
  Cons(T, Box<List<T>>),
  Nil,
}
```


### `Rc<T>` immutable reference counter 

The `Rc<T>` type keeps track of the number of references to a value to determine whether or not the value is still in use.

> Note that `Rc<T>` is only for use in **single-threaded** scenarios.

> Note `Rc<T>` is immutable. 

We use the `Rc<T>` type when we want to _allocate some data on the heap_ for multiple parts of our program to read and _we can’t determine at compile time which part will finish using the data last_. 

> If we knew which part would finish last, we could just make that part the data’s owner, and the normal ownership rules enforced at compile time would take effect.


```rust
enum List<T> {
    Cons(T, Rc<List<T>>),
    Nil,
}

use crate::List::{Cons, Nil};
use std::rc::Rc;

fn main() {
  let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
  let b = Cons(3, Rc::clone(&a));
  let c = Cons(4, Rc::clone(&a));
  
  let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
  println!("count after creating a = {}", 
           Rc::strong_count(&a));  // initial 1
  let b = Cons(3, Rc::clone(&a));
  println!("count after creating b = {}",
            Rc::strong_count(&a)); // 2
  {
    let c = Cons(4, Rc::clone(&a));
    println!("count after creating c = {}",
             Rc::strong_count(&a)); // 3
  }
  println!("count after c goes out of scope = {}",
           Rc::strong_count(&a));  // 2
}

```

- `Rc::clone`  reference count + 1; do not copy the data hold by the pointer


### `RefCell<T>` Mutable reference count 

> With references and `Box<T>`, the [[#Borrowing rules]]’ invariants are enforced at compile time. With `RefCell<T>`, these invariants are enforced _at runtime_.

> The `RefCell<T>` type is useful when you’re sure your code follows the borrowing rules but the compiler is unable to understand and guarantee that.

> Similar to `Rc<T>`, `RefCell<T>` is only for use in single-threaded scenarios and will give you a compile-time error if you try using it in a multithreaded context.

Because `RefCell<T>` allows mutable borrows checked at runtime, _you can mutate the value inside the `RefCell<T>` even when the `RefCell<T>` is immutable_.


### Atomic Reference Count `Arc<T>`

`Arc<T>` _is_ a type like `Rc<T>` that is safe to use in concurrent situations.

## Common Collections `std::collections`

[std::collections - Rust (rust-lang.org)](https://doc.rust-lang.org/std/collections/index.html)

### Vector
```rust

// create vector

let v: Vec<i32> = Vec::new();       // Vec::new function
let v = vec![1, 2, 3];              // use the vec! macro
let mut v: Vec<i32> = Vec::new();   // mutable vector
v.push(1); v.push(2); v.push(3);


{
  let v = vec![1, 2, 3];
  // When the vector gets dropped, 
  //   all of its contents are also dropped,
  //   meaning those integers it holds will be cleaned up.
}

// accessing elements
let third: &i32 = &v[2];
println!("The third element is {}", third);
// Vec::get returns an Option<&T>
match v.get(2) {
  Some(third) => println!("The third element is {}", third),
  None => println!("There is no third element."),
}
let does_not_exist = &v[100];  // panic
let does_not_exist = v.get(100);



// mutable and immutable ref of vec
let mut v = vec![1, 2, 3, 4, 5]; 
let first = &v[0];     // immutable ref to v[0]
v.push(6);             // mutates v
// panic after access to immutable ref after
//   the vec is mutated
println!("The first element is: {}", first);

// loop a vector
for i in &v { 
  println!("{}", i); 
}

// loop and mutates value
let mut v = vec![100, 32, 57]; 
for i in &mut v { 
  *i += 50; 
}
```

### String

- `str`, `&str`: the String [[#Slice]]
- `String`: from std lib, growable, mutable, owned, UTF-8 encoded string type

Many of the same operations available with `Vec<T>` are available with `String` as well

```rust
let mut s = String::new();      // new empty string
let data = "initial contents";  // string literal
s = data.to_string();           // trait Display
let s = String::from("你好");


// update string
let mut s = String::from("foo"); // take string slice
s.push_str("bar");
s.push(' ');                     // push take a single char


// concat
let s1 = String::from("Hello, "); 
let s2 = String::from("world!"); 
// note s1 has been moved here and can no longer be used
let s3 = s1 + &s2;

let s1 = String::from("tic"); 
let s2 = String::from("tac");
let s3 = String::from("toe");
// `format!` macro uses references
let s = format!("{}-{}-{}", s1, s2, s3);


for c in "नमस्ते".chars() {
    println!("{}", c);
}

for b in "नमस्ते".bytes() {
    println!("{}", b);
}

// A `String` is a wrapper over a `Vec<u8>`
//  it can not be indexed.
let h = s1[0];     // panic
```


### Hash map

```rust
use std::collections::HashMap;

let mut scores = HashMap::new();
scores.insert(String::from("Blue"), 10);
scores.insert(String::from("Yellow"), 50);

let teams = vec![String::from("Blue"), String::from("Yellow")]; let initial_scores = vec![10, 50]; 
// use the `collect` method to turn 
//   iterator of tuples into a hash map
let mut scores: HashMap<_, _> = 
    teams.into_iter().zip(initial_scores.into_iter()).collect();


// For types that implement the `Copy` trait, like `i32`,
//   the values are copied into the hash map. 
// For owned values like `String`, the values will be moved
// For references to values into the hash map,
//   the values won’t be moved into the hash map. 
//   The values that the references point to 
//   must be valid for at least as long as the
//   hash map is valid.


let team_name = String::from("Blue");
let score:Option<&i32> = scores.get(&team_name);
for (key, value) in &scores {
  println!("{}: {}", key, value); 
}

// insert if not exists
scores.entry(String::from("Blue")).or_insert(50);
```

## IO

### cmd line args
`let args: Vec[String] = std::env::args().collect()`

### File
- `std::fs`
  - `std::fs::read_to_string(path:&str)`

## Testing

> “Program testing can be a very effective way to show the presence of bugs, but it is hopelessly inadequate for showing their absence.”

When you run your tests with the `cargo test` command, Rust builds a test runner binary that runs the functions _annotated with the `test` attribute_ and reports on whether each test function passes or fails.


The Rust community thinks about tests in terms of two main categories: _unit tests_ and _integration tests_. 

- **Unit tests** are small and more focused, testing one module in isolation at a time, and can test private interfaces. 
- **Integration tests** are _entirely external_ to your library and use your code in the same way any other external code would, using only the public interface and potentially exercising multiple modules per test


### Unit Test

> You’ll put unit tests in the _src_ directory in each file with the code that they’re testing.

The convention is to create a module named `tests` in each file to contain the test functions and to annotate the module with `cfg(test)`

```rust
// src/lib.rs

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
  #[test]
  fn another() {
    panic!("Make this test fail");
  }
  
  // add fail info
  #[test]
  fn greeting_contains_name() {
    let result = greeting("Carol");
    assert!(
      result.contains("Carol"),
      "Greeting did not contain name, value was `{}`",
      result
    );
  }
  
  // test for inputs that should panic
  #[test]
  #[should_panic(expected = "panic message")]
  fn greater_than_100() {
      Guess::new(200);
  }
  
  // ignore tests
  #[test] 
  #[ignore] 
  fn expensive_test() {  } }
```

- `#[test]` attribute indicates its a test
- `assert_eq!`, `assert_ne!`: When the assertions fail, these macros print their arguments using debug formatting, which means the values being compared must implement the `PartialEq` and `Debug` traits.
- `assert!(expr: bool);`

### Integration Test

> We create a _tests_ directory at the top level of our project directory, next to _src_. Cargo will compile each of the files as an individual crate.

We don’t need to annotate any code in _tests/integration_test.rs_ with `#[cfg(test)]`.


### Document Test

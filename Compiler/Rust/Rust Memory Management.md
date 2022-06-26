#Rust
# Memory Management

> Rust uses a third approach: memory is managed through a system of ownership with a set of rules that the compiler checks at compile time. None of the ownership features slow down your program while it’s running.

## Rules

-   (**one**) Each value in Rust has a variable that’s called its _owner_.
-   (**and only one**) There can only be one owner at a time.
-   (**drop**) When the owner goes out of scope, the value will be dropped.

## Scope

> The variable is valid from the point at which it’s declared until the end of the current _scope_. this is similar to [[Resource Acquisition Is Initialization (RAII)]]

# Ownership

### Move
```rust
let s1 = String::from("hello");
// ownership move from s1 to s2
let s2 = s1;  
// from now on `s1` no longer be valid
println!("{}, world!", s1); // panic using an invalid reference
```


>  If a type implements the `Copy` trait, an older variable is still usable after assignment. Rust won’t let us annotate a type with the `Copy` trait if the type, or any of its parts, has implemented the `Drop` trait.

> Passing a variable to a function will move or copy, just as assignment does.

> Returning values can also transfer ownership.

> `match x {}` also moves x 



## Reference
a reference of an object just borrow the object and will not move the ownership.

> _reference_ refers to an object but not owns it. As in real life, if a person owns something, you can borrow it from them. When you’re done, you have to give it back.

![[rust-memory-reference.svg]]

```rust
fn main() {
  let mut s1 = String::from("hello");
  let len = calculate_length(&s1);
  println!("The length of '{}' is {}.", s1, len);
  
  change(&mut s1);
  println!(s1);
}

fn calculate_length(s: &String) -> usize { // s is a reference to a String
    s.len()
} // Here, s goes out of scope. But because it does not have ownership of what it refers to, nothing happens.

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```


##### Borrowing rules
-   At any given time, you can have _either_ one mutable reference _or_ any number of immutable references.
-   References must always be valid.

> You can have only one mutable reference to a particular piece of data in a particular scope. The benefit of having this restriction is that Rust can prevent _data races_ at compile time. We _also_ cannot have a mutable reference while we have an immutable one.

> In Rust, by contrast, the compiler guarantees that _references will never be dangling_: if you have a reference to some data, the compiler will ensure that the data will not go out of scope before the reference to the data does.

>[!NOTE] Inherited Mutability
>
>In Rust, by default, mutability of fields of structs are _inherited_ from the mutability of the struct object.


##### Lifetime of references

Notice Rust guarantees reference are all valid, i.e. no dangling references, by borrow checker and lifetime analysis.

```rust
// lifetime notation
//   lifetime of x, y, and return are all at least 'a
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
  if x.len() > y.len() { 
    x 
  } else { 
    y 
  } 
}


impl<'a> ImportantExcerpt<'a> { 
  fn announce_and_return_part(&self, announcement: &str) -> &str
  { 
    println!("Attention please: {}", announcement);
    self.part 
  } 
}

// static lifetime
let s: &'static str = "I have a static lifetime.";
```


```rust
// all together

use std::fmt::Display;

fn longest_with_an_announcement<'a, T>(
    x: &'a str,
    y: &'a str,
    ann: T,
) -> &'a str
where
    T: Display,
{
    println!("Announcement! {}", ann);
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```


###### Lifetime Elision

Lifetimes on function or method parameters are called _input lifetimes_, and lifetimes on return values are called _output lifetimes_.

1. Each _parameter_ that is a reference gets its own lifetime parameter.
2. If there is _exactly one input lifetime parameter_, that lifetime is assigned to all output lifetime parameters.
3. If there are multiple input lifetime parameters, but one of them is `&self` or `&mut self` because this is a method, the lifetime of `self` is assigned to _all output lifetime parameters_.

###### FAQ
[Common rust lifetime misconceptions](https://github.com/pretzelhammer/rust-blog/blob/master/posts/common-rust-lifetime-misconceptions.md)


## Slice

Slices also do not owns the data.

```rust
fn main() {
    let s = String::from("hello world");
    let hello: &str = &s[0..5];  // [0,5)
    let world: &str = &s[6..11]; // &str is the type of str slice
    
    let s_slice: &str = "Hello world";
}
```

![[rust-memory-slice.svg]]


## Deref Coercion

__Deref coercion__ is a convenience that Rust _performs on arguments to functions and methods_.

Deref coercion converts such a type _into a reference_ to another type.

A sequence of calls to the `deref` method converts the type we provided into the type the parameter needs.

Deref coercion can convert `&String` to `&str` because `String` implements the `Deref` trait such that it returns `&str`

Rust does __deref coercion__ when it finds types and trait implementations in three cases:
-   From `&T` to `&U` when `T: Deref<Target=U>`
-   From `&mut T` to `&mut U` when `T: DerefMut<Target=U>`
-   From `&mut T` to `&U` when `T: Deref<Target=U>`

See also [[#Deref]]


# Smart Pointer and Reference counting

The concept of smart pointers isn’t unique to Rust: smart pointers originated in C++ and exist in other languages as well.
In Rust, the different smart pointers defined in the standard library provide functionality beyond that provided by references.

> Smart pointer enables you to have _multiple owners_ of data by keeping track of the number of owners and, when no owners remain, cleaning up the data.

> References are pointers that only _borrow_ data; in contrast, in many cases, smart pointers _own_ the data they point to.

Common _smart pointers_ in the standard library:
- `Box<T>` for allocating values on the heap
- `Rc<T>`, a reference counting type that enables multiple ownership
- `Ref<T>` and `RefMut<T>`, accessed through `RefCell<T>`, a type that enforces the borrowing rules at runtime instead of compile time
- `String` is also a smart pointer.


###### Comparison of these pointers:

| Pointer Types           | Owners | Borrow check time | N.B.                                  |
| ----------------------- | ------ | ----------------- | ------------------------------------- |
| `Box<T>` [^box]         | $1$    | compile time      |                                       |
| `Rc<T>`   [^rc]         | $n$    | compile time      | multi-owner makes inner obj immutable |
| `RefCell<T>` [^refcell] | $1$    | runtime           |                                       |
| `Arc<T>`                | $n$    | compile time      | async version of `Rc`                 |
| `Rc<RefCell<T>>`        | $n$    | runtime           |                                       |

[^rc]:  `Rc<T>` enables multiple owners of the same data; `Box<T>` and `RefCell<T>` have single owners.
[^box]:  `Box<T>` allows immutable or mutable borrows checked at compile time; `Rc<T>` allows only immutable borrows checked at compile time; `RefCell<T>` allows immutable or mutable borrows checked at runtime.
[^refcell]: Because `RefCell<T>` allows mutable borrows checked at runtime, _you can mutate the value inside the `RefCell<T>` even when the `RefCell<T>` is immutable_. See also [`std::cell`](https://doc.rust-lang.org/std/cell/index.html)



###### Inherited mutability and Interior mutability

The [std::cell](https://doc.rust-lang.org/std/cell/index.html)crate defines the type `Cell` and `RefCell` supports _interior mutability_:
- `Cell<T>` implements interior mutability by _moving values in and out_ of the `Cell<T>`;
- `RefCell<T>` uses Rust’s lifetimes to implement _'dynamic borrowing'_, a process whereby one can claim temporary, exclusive, mutable access to the inner value.


Common places to use _interior mutability_:  
- Introducing mutability ‘inside’ of something immutable;
- Implementation details of logically-immutable methods;
- Mutating implementations of [`Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html "Clone").




### Deref, Drop and others

The characteristic that distinguishes a smart pointer from an ordinary struct is that smart pointers implement the `Deref` and `Drop` traits.
- The `Deref` trait allows an _instance_ of the smart pointer struct to _behave like a reference_ so you can write code that works with either references or smart pointers.
- The `Drop` trait allows you to customize the code that is run when an instance of the smart pointer _goes out of scope_.


###### Deref

```rust
pub trait Deref {
    type Target: ?Sized;
    fn deref(&self) -> &Self::Target;
}

pub trait DerefMut: Deref {
    fn deref_mut(&mut self) -> &mut Self::Target;
}
```

1. If `T` implements `Deref<Target=U>`, and `x` is a value of type `T`, then:
    - In _immutable_ contexts, `*x` (where `T` is neither a reference nor a raw pointer) is equivalent to `*Deref::deref(&x)`.
    - Values of type `&T` are coerced to values of type `&U`
    - `T` _implicitly implements_ all the (immutable) methods of the type `U`.
2. If `T` implements `DerefMut<Target=U>`, and `x` is a value of type `T`, then:
    - In _mutable_ contexts, `*x` (where `T` is neither a reference nor a raw pointer) is equivalent to `*DerefMut::deref_mut(&mut x)`.
    - Values of type `&mut T` are coerced to values of type `&mut U`
    - `T` _implicitly implements_ all the (mutable) methods of the type `U`.

See also [[#Deref Coercion]] and [Deref in std::ops - Rust (rust-lang.org)](https://doc.rust-lang.org/std/ops/trait.Deref.html)


###### Drop

[Drop in std::ops - Rust (rust-lang.org)](https://doc.rust-lang.org/std/ops/trait.Drop.html)

```rust
pub trait Drop {
    fn drop(&mut self);
}
```

When a value is no longer needed, Rust will run a “**destructor**” on that value. 
This **destructor** consists of _two components_:
1. A call to `Drop::drop` for that value, if this special `Drop` trait is implemented for its type.
2. The automatically generated “drop glue” which _recursively (in a pre-order)_ calls the destructors of all the fields of this value.
        1. The drop order of _local variables_ is just like the `defer` statement in Golang, i.e., reverse order of variable declaration.
        2. The drop order of _structs_ is the order of the field declaration.


###### Copy and Clone

```rust
pub trait Copy: Clone { }
```

Differences of  the`Copy` and `Clone` trait
- Copies happen implicitly, e.g., `y=x`. A type can implement `Copy` if all of its components implement `Copy`, any type implementing [[#Drop]] can’t be `Copy`
- Cloning is an explicit action, `x.clone()`

These are `Copy` types:
- _Function item_ types (i.e., the distinct types defined for each function)
- _Function pointer_ types (e.g., `fn() -> i32`)
- _Tuple_ types, if each component also implements `Copy` (e.g., `()`, `(i32, bool)`)
- _Closure_ types, if they capture no value from the environment or if all such captured values implement `Copy` themselves. Note that variables captured by shared reference always implement `Copy` (even if the referent doesn’t), while variables captured by mutable reference never implement `Copy`.


# Pin 

>[!TLDR]
> A `Pin<P>` ensures that the **pointee** of any *pointer type* `P` has a stable location in memory, meaning it _cannot be moved_ elsewhere and its memory _cannot be deallocated until it gets dropped_.
> 
> _Its memory will not get invalidated or repurposed from the moment it gets pinned until when `drop  is called_. 
> Only once `drop` returns or panics, the memory may be reused.

Common [[#Smart Pointer and Reference counting|smart pointers]] (`Box<T>`, `&mut T` etc.) allow _replacing_ and _moving_ the values they contain:
```rust
let boxed = Box::new(32);
let moved = if nightly { 
    boxed.into_inner() // nightly-only
  } else {
    *boxed
  }
```

 See Also:
 - [`std::pin`](https://doc.rust-lang.org/std/pin/index.html)
 - [`pin_utils`](https://docs.rs/pin-utils/latest/pin_utils/)
 - [Pin, Unpin, and why Rust needs them](https://blog.cloudflare.com/pin-and-unpin-in-rust/)
 - [Pinning - Asynchronous Programming in Rust](https://rust-lang.github.io/async-book/04_pinning/01_chapter.html)
 - [Future in `std::future`](https://doc.rust-lang.org/std/future/trait.Future.html)



# Phantom Data

`std::marker::PhantomData<T>`

> Zero-sized type used to mark things that “act like” they own a `T`.


[PhantomData - The Rustonomicon (rust-lang.org)](https://doc.rust-lang.org/nomicon/phantom-data.html)

[PhantomData in std::marker - Rust (rust-lang.org)](https://doc.rust-lang.org/std/marker/struct.PhantomData.html)

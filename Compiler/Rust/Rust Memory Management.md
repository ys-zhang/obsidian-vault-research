#Rust
# Memory Management: Ownership

> Rust uses a third approach: memory is managed through a system of ownership with a set of rules that the compiler checks at compile time. None of the ownership features slow down your program while it’s running.

## Rules

-   (**one**) Each value in Rust has a variable that’s called its _owner_.
-   (**and only one**) There can only be one owner at a time.
-   (**drop**) When the owner goes out of scope, the value will be dropped.

## Scope

> The variable is valid from the point at which it’s declared until the end of the current _scope_. this is similar to [[Resource Acquisition Is Initialization (RAII)]]

## Ownership

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

### Borrow 

#### Reference
a reference of an object just borrow the object and will not move the ownership.

> reference refers to an object by not owns it. As in real life, if a person owns something, you can borrow it from them. When you’re done, you have to give it back.

![[Pasted image 20211020173723.png | 400]]
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
} // Here, s goes out of scope. But because it does not have ownership of what
  // it refers to, nothing happens.

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```

##### Borrowing rules
-   At any given time, you can have _either_ one mutable reference _or_ any number of immutable references.
-   References must always be valid.

> You can have only one mutable reference to a particular piece of data in a particular scope. The benefit of having this restriction is that Rust can prevent _data races_ at compile time. We _also_ cannot have a mutable reference while we have an immutable one.

> In Rust, by contrast, the compiler guarantees that references will never be dangling references: if you have a reference to some data, the compiler will ensure that the data will not go out of scope before the reference to the data does.


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


#### Slice
Slices also do not owns the data.

```rust
fn main() {
    let s = String::from("hello world");
    let hello: &str = &s[0..5];  // [0,5)
    let world: &str = &s[6..11]; // &str is the type of str slice
    
    let s_slice: &str = "Hello world";
}
```

![[Pasted image 20211020180809.png]]



### Deref Coercion
_Deref coercion_ is a convenience that Rust _performs on arguments to functions and methods_.

Deref coercion converts such a type into a reference to another type.

A sequence of calls to the `deref` method converts the type we provided into the type the parameter needs.

Deref coercion can convert `&String` to `&str` because `String` implements the `Deref` trait such that it returns `&str`

Rust does _deref coercion_ when it finds types and trait implementations in three cases:
-   From `&T` to `&U` when `T: Deref<Target=U>`
-   From `&mut T` to `&mut U` when `T: DerefMut<Target=U>`
-   From `&mut T` to `&U` when `T: Deref<Target=U>`


See [[#Memory related traits structs]]


## Smart Pointer and _reference counting_

The concept of smart pointers isn’t unique to Rust: smart pointers originated in C++ and exist in other languages as well.
In Rust, the different smart pointers defined in the standard library provide functionality beyond that provided by references.

> Smart pointer enables you to have _multiple owners_ of data by keeping track of the number of owners and, when no owners remain, cleaning up the data.

> References are pointers that only _borrow_ data; in contrast, in many cases, smart pointers _own_ the data they point to.


The characteristic that distinguishes a smart pointer from an ordinary struct is that smart pointers implement the `Deref` and `Drop` traits.
- `Deref` trait allows an instance of the smart pointer struct to behave like a reference so you can write code that works with either references or smart pointers.
- The `Drop` trait allows you to customize the code that is run when an instance of the smart pointer goes out of scope.

Common smart pointers in the standard library:
-   `Box<T>` for allocating values on the heap
-   `Rc<T>`, a reference counting type that enables multiple ownership
-   `Ref<T>` and `RefMut<T>`, accessed through `RefCell<T>`, a type that enforces the borrowing rules at runtime instead of compile time

Comparison of these pointers:
-   `Rc<T>` enables multiple owners of the same data; `Box<T>` and `RefCell<T>` have single owners.
-   `Box<T>` allows immutable or mutable borrows checked at compile time; `Rc<T>` allows only immutable borrows checked at compile time; `RefCell<T>` allows immutable or mutable borrows checked at runtime.
-   Because `RefCell<T>` allows mutable borrows checked at runtime, _you can mutate the value inside the `RefCell<T>` even when the `RefCell<T>` is immutable_.

`String` is also a smart pointer.




# Misc

## Phantom Data

`std::marker::PhantomData<T>`

> Zero-sized type used to mark things that “act like” they own a `T`.


[PhantomData - The Rustonomicon (rust-lang.org)](https://doc.rust-lang.org/nomicon/phantom-data.html)

[PhantomData in std::marker - Rust (rust-lang.org)](https://doc.rust-lang.org/std/marker/struct.PhantomData.html)

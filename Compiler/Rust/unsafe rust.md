#Rust 

An _unsafe feature_ is one that imposes a contract: rules that Rust cannot enforce automatically, but which you must nonetheless follow to avoid _undefined behavior_.


### Unsafe block

An unsafe block unlocks five additional options for you:
1. You can call _unsafe functions_. Each unsafe function must specify its own contract, depending on its purpose.
2. You can dereference _raw pointers_.
3. You can access the _fields of unions_.
4. You can access _mutable `static` variables_.
5. You can access functions and variables declared through Rust’s _foreign function interface_.


### Unsafe trait

An _unsafe trait_ is a trait that has a contract Rust cannot check or enforce that implementers must satisfy to avoid undefined behavior.

A function that bounds its type variables with an _unsafe trait_ is typically one that uses unsafe features itself, and _satisfies their contracts only by depending on the unsafe trait’s contract_.

###### Examples
1. `std::marker::Send`, requires implementers to be safe to move to another thread.
2. `std::marker::Sync`, requires implementers to be safe to share among threads via shared references.
3. `core::nonzero::Zeroable`, for types that can be safely initialized by setting all their bytes to zero.


### Raw pointers

A raw pointer in Rust is an unconstrained pointer.

> Unlike references, _raw pointers_ are neither `Send` nor `Sync`. There is nothing inherently unsafe about sending or sharing raw pointers between threads; after all, wherever they go, you still need an unsafe block to dereference them.

There is no plain `*T` type; you must always specify either `const` or `mut`:
1. `*const T`  constant pointer;
2. `*mut T`  mutable pointer 



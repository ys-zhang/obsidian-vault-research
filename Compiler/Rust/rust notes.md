#Rust 

- `extern crate $crate_name`
##### Allocate memory on heap:
  - `Box::new` is the simplest way to allocate a value in the _heap_.
  - A vector `Vec<T>` is a resizable array of elements of type `T`, allocated on the _heap_.
  - A `String` has a resizable buffer holding UTF-8 text on the _heap_.

##### string types:
  - A `String` has a resizable buffer holding UTF-8 text on the _heap_.
  - A `&str` (pronounced “_stir_” or “string slice”) is a reference to a run of UTF-8 text owned by someone else: it “borrows” the text. You can think of a `&str` as being nothing more than a &[u8] that is guaranteed to hold well-formed UTF-8.
  - A `String` or `&str`’s `.len()` method returns its length. **The length is measured in bytes, not characters**.

  - Stick to `String` and `&str` for Unicode text. 
  - When working with filenames, use `std::path::PathBuf` and `&Path` instead. 
  - When working with binary data that isn’t UTF-8 encoded at all, use `Vec` and `&[u8]`.
  - When working with environment variable names and command-line arguments in the native form presented by the operating system, use `OsString` and `&OsStr`. 
  - When interoperating with C libraries that use `null` terminated strings, use `std::ffi::CString` and `&CStr`.
  
  
##### The Copy trait

The standard Copy types include all the machine integer and floating-point numeric types, the `char` and `bool` types, and a few others. **A tuple or fixed-size array of Copy types is itself a Copy type.**

> Copies happen implicitly, for example as part of an assignment `y = x`. The behaviour of `Copy` is not overloadable; **it is always a simple bit-wise copy**.

> A type can implement `Copy` if all of its components implement `Copy`.

> Generalising the latter case, any type implementing `Drop` can’t be `Copy`, because it’s managing some resource besides its own `size_of::<T>` bytes.

> Generally speaking, if your type _can_ implement `Copy`, it should. Keep in mind, though, that implementing `Copy` is part of the public API of your type.
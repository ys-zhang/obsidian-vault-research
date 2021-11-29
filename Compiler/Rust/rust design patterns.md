[ Rust Design Patterns](https://rust-unofficial.github.io/patterns/)

# Idioms

- use borrowed types over borrowing owned types for arguments.
    - `&str` over `&String`  
    - `&[T]` over `&Vec<T>`
    - `&T` over `&Box<T>`

- [Learning Rust With EntirelyToo Many Linked Lists](https://rust-unofficial.github.io/too-many-lists/index.html)

# Pointer Types

##  `Box<T>`

![[Rust#`Box<T>`]]



## `Rc<T>`

![[Rust#`Rc<T>` immutable reference counter]]


# Memory Cell

Cells are **shareable mutable containers**, which allows bypassing the _Rust Memory Safety Rule_, either:
- (_aliasing_) multiple _immutable/shared references_ (`&T`) to an object with type `T`, or
- (_mutability_) single _mutable/unique reference_ (`&mut T`) to an object with type `T`.

Values of the `Cell<T>` and `RefCell<T>` types may be mutated through _shared references_ (i.e. the common `&T` type), whereas most Rust types can only be mutated through unique (`&mut T`) references. We say that `Cell<T>` and `RefCell<T>` provide **‘interior mutability’,** in contrast with typical Rust types that exhibit ‘inherited mutability’.

1. `Cell<T>` implements interior mutability by _moving values in and out_ of the `Cell<T>`.
2. `RefCell<T>` implements interior mutability by _borrow values living inside the cell_

> One difference is that `RefCell` checks `borrow(), borrow_mut(), replace()` at runtime and these methods may panic.


> `Cell<T>` is just a different way to accomplish interior mutability. With `RefCell<T>`, you get a reference to the underlying data (note the `Ref/RefMut` part). With `Cell<T>`, you indirectly interact with the wrapped value by using `set` and `get` (and some other fancy methods).


Differences:
1. types wrapped in `Cell` must implement the `Copy` trait while those in `RefCell` don’t need to. This makes sense. When calling `get` on a `Cell`, you are getting a copy of the wrapped data, whereas the methods associated with `RefCell` are `borrow` and `borrow_mut`, which return references to the underlying data.
2. `RefCell`’s references are checked at runtime, which comes at a performance cost of verifying reference counts and possibly being completely wrong about your borrowing logic and causing your program to panic (not good); you can’t cause panics using `set`and `get` with `Cell` as you’re not modifying the data through reference


## `Cell<T>`

Cell allows you to mutate, i.e. replace **value** in the cell, with an immutable reference to the cell.

1. `get(&self)` method returns a _copy_ of the contained value
2. `get_mut(&mut self)` method borrows the contained data by _comply to the rust memory safety rule_ (checked by the borrow checker at compile time), i.e., _only once mutable ref can exist_

<iframe width="100%" height="430" src="https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&code=use%20std%3A%3Acell%3A%3ACell%3B%0A%0Afn%20main()%20%7B%0A%20%20%20%20let%20cell%20%3D%20Cell%3A%3Anew(1)%3B%20%20%2F%2F%20%3C--%20notice%20no%20%60mut%60%20here%0A%20%20%20%20let%20r1%20%3D%20%26cell%3B%0A%20%20%20%20println!(%22%7B%3A%3F%7D%22%2C%20r1)%3B%20%20%2F%2F%20prints%201%3B%0A%20%20%20%20cell.replace(2)%3B%0A%20%20%20%20let%20r2%20%3D%20%26cell%3B%0A%20%20%20%20println!(%22%7B%3A%3F%7D%22%2C%20r1)%3B%20%20%2F%2F%20prints%202%3B%0A%20%20%20%20r1.replace(3)%3B%0A%20%20%20%20println!(%22%7B%3A%3F%7D%22%2C%20r2)%3B%20%20%2F%2F%20prints%203%3B%0A%7D" title="YouTube video player" frameborder="0"  allowfullscreen></iframe>

The following will not compile 

```rust
use std::cell::Cell;

fn main() {
    let mut cell = Cell::new(1);  // <-- add `mut` to allow borrow mut later
    let r = cell.get_mut();       // <-- borrow as mut
    println!("{:?}", r);
    cell.replace(2);              // <-- fail due to existing mut ref
    println!("{:?}", r);
}
```


## `RefCell<T>`

![[Rust#`RefCell<T>` Mutable reference count]]

`RefCell<T>` uses Rust’s lifetimes to implement **‘dynamic borrowing’,** a process whereby one can claim temporary, exclusive, mutable access to the inner value. 
Borrows for `RefCell<T>`s are _tracked ‘at runtime’,_ unlike Rust’s native reference types which are entirely tracked statically, at compile time. 
Because `RefCell<T>` borrows are dynamic it is possible to attempt to borrow a value that is already mutably borrowed; when this happens it results in thread panic.

methods:
- `RefCell<T>::borrow(&self) -> Ref<'_, T>`
- `RefCell<T>::borrow_mut(&self) -> RefMut<'_, T>`

<iframe width="100%" height="630" src="https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&code=use%20std%3A%3Acell%3A%3ARefCell%3B%0A%0A%23%5Bderive(Debug)%5D%0Astruct%20A%20%7B%0A%20%20%20%20m%3A%20%5Busize%3B%201%5D%2C%0A%20%20%20%20n%20%3A%20%5Busize%3B%201%5D%0A%7D%0A%0A%23%5Ballow(unused)%5D%0Aimpl%20A%20%7B%0A%20%20%20%20fn%20m(%26self)%20-%3E%20usize%20%7B%20self.m%5B0%5D%20%7D%0A%20%20%20%20fn%20n(%26self)%20-%3E%20usize%20%7B%20self.n%5B0%5D%20%7D%0A%20%20%20%20%0A%20%20%20%20fn%20set_m(%26mut%20self%2C%20value%3A%20usize)%20%20%7B%20self.m%5B0%5D%20%3D%20value%20%7D%0A%20%20%20%20fn%20set_n(%26mut%20self%2C%20value%3A%20usize)%20%20%7B%20self.n%5B0%5D%20%3D%20value%20%7D%0A%7D%0A%0Afn%20main()%20%7B%0A%20%20%20%20let%20refcell%20%3D%20RefCell%3A%3Anew(A%20%7Bm%3A%20%5B0%5D%2C%20n%3A%20%5B1%5D%7D)%3B%0A%20%20%20%20println!(%22%7B%3A%23%3F%7D%22%2C%20refcell)%3B%0A%20%20%20%20let%20m%20%3D%20refcell.borrow().m()%3B%0A%20%20%20%20refcell.borrow_mut().set_n(m)%3B%0A%20%20%20%20println!(%22%7B%3A%23%3F%7D%22%2C%20refcell)%3B%0A%7D" title="YouTube video player" frameborder="0"  allowfullscreen></iframe>
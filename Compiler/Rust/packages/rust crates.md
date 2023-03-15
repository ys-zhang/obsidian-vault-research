#Rust 
#Package



[awesome rust](https://awesome-rust.com/)



# Logging

#### `tracing`

[`tracing`](https://docs.rs/tracing/latest/tracing/index.html) is a framework for _instrumenting_ Rust programs to collect structured, _event-based_ diagnostic information. 

>[!PROBLEM]
> Interpreting linear logging record is difficult, since timestamps cannot prove _happen before relation_ or _causality_.
> 
> `tracing` solves this problem by introducing _structured events_ — `Span`

```rust
use tracing::{event, span, Level};

// records an event outside of any span context:
event!(Level::INFO, "something happened");

let span = span!(Level::INFO, "my_span");

// `enter` returns a RAII guard which, when dropped, exits the span. this indicates that we are in the span for the current lexical scope.
let _guard = span.enter();

// records an event within "my_span".
event!(Level::DEBUG, "something happened inside my_span");
```

| Traits       | Meaning                                      |
| ------------ | -------------------------------------------- |
| `Span`       | a _period_ of time                           |
| `Event`      | a _moment_ in time (usually within a `Span`) |
| `Subscriber` | triggered by `Event` or enter/exit a `Span`  |

| Macros/Methods            | Meaning                           |
| ------------------------- | --------------------------------- |
| `#[intrument]`            | add `tracing` spans to functions  |
| `span!()`                 | create a `Span` RAII guard        |
| `Span::in_scope(closure)` | wrap synchronous code in a `Span` |
| `event!(level, msg)`      | trigger an `Event`                |
|                           |                                   |


# Async Programming

- [rust thread model](https://doc.rust-lang.org/std/thread/index.html)


- [`async_stream`](https://docs.rs/async-stream/latest/async_stream/) is a crate creating [Streams](https://rust-lang.github.io/async-book/05_streams/01_chapter.html) base on `tokio`, noting [generators](https://doc.rust-lang.org/beta/unstable-book/language-features/generators.html) is still an unstable feature.
- [`async_channel`](https://docs.rs/async-channel/latest/async_channel/) 
- [`tokio` - An asynchronous Rust runtime](https://tokio.rs/)
- [`fparking_lot` - A faster implementation of sync tools](https://docs.rs/parking_lot/0.12.1/parking_lot/index.html)
- [`crossbeam`- Tools for concurrent programming](https://docs.rs/crossbeam/latest/crossbeam/index.html)


# Serialize

#### `serde`  

[Overview · Serde](https://serde.rs/)

> Serde is a framework for serializing and deserializing Rust data structures efficiently and generically.


##### Issues

1. Serde cannot automatically serialize/deserialize large array. 
    [[Solved] Serialization How do I use Serde to (de)serialize arrays greater than 32 elements, such as [u8; 128]? - Code Redirect](https://coderedirect.com/questions/301694/how-do-i-use-serde-to-deserialize-arrays-greater-than-32-elements-such-as-u8)

 


# GUI

[Welcome! - The Nannou Guide](https://guide.nannou.cc/welcome.html)




# Collections

#### `tuple_list`
  Crate for macro-free variadic tuple metaprogramming.

```rust
/// tuple_list!
assert_eq!(
  tuple_list!(10, false, "foo");
  (10, (false, ("foo", ()))),
)
  ```


# Command line

#### `clap`


# Database

[Diesel is a Safe, Extensible ORM and Query Builder for Rust](http://diesel.rs/)



# FFI

[rust-lang/rust-bindgen: Automatically generates Rust FFI bindings to C (and some C++) libraries. (github.com)](https://github.com/rust-lang/rust-bindgen)
[eqrion/cbindgen: A project for generating C bindings from Rust code (github.com)](https://github.com/eqrion/cbindgen)


# Misc

- [`slotmap`](https://docs.rs/slotmap/latest) is a perfect tool to adopt the design from [this blog](https://kyren.github.io/2018/09/14/rustconf-talk.html)


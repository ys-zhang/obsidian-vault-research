#wasm
#Rust 
#javascript 

# Import JsValue(managed by JS runtime) to Rust

![[wasm-bindgen-js2rust.excalidraw|800]]

Three methods can be used:
1. `JsValue` from `wasm_bindgen` library
2. `serde-wasm-bindgen` support
3. use json and directly through `serde` or use the crate `gloo-utils`
4. 

## serde-wasm-bindgen

```rust
// convert to JsValue
let rust = SomeType { ... };
let js: JsValue = serde_wasm_bindgen::to_value(rust).unwrap();

  
// from JsValue
let mut js: JsValue;
let rust: SomeType = serde_wasm_bindgen::from_value(js).unwrap(); 
```


# Export to JS (managed by WASM)

> By default, structs exported from Rust become JavaScript classes with a single `ptr` property. All other properties are implemented as getters


# Internals

The goal of `wasm-bindgen` is to enhance the "ABI" of WASM modules with richer types like classes, JS objects, Rust structs, strings, etc.

## Generated File Structure
```bash
foo.js       # the desired JS interface
foo_bg.wasm  # lightly modified from orginal rustc generations
```

## String passing
- JS to Rust
    1. encode string to `u8` array in js;
    2. copy the `u8` array into WASM heap
- Rust to JS
    1. copy out string into JS from WASM
    2. free wasm memory

## Type Conversion

### Rust -> JS
To make a type convertible to Javascript it must implement the following trait
```rust
pub trait IntoWasmAbi: WasmDescribe {
  /* 
    The Abi type is what eventually can be accessed 
    from JS.
    WasmAbi is a trait bound,i.e., types directly supported
    in WASM including: 
    - WASM primitives like u8, f64;
    - basic `repr(C)` structs
   */
  type Abi: WasmAbi;  
  fn into_abi(self) -> Self::Abi;
}
```
The trait is used in 
1. convert return values of rust exported functions to JS
2. convert arguments to (rust imported) JS functions.

### JS -> Rust
 see https://rustwasm.github.io/wasm-bindgen/contributing/design/rust-type-conversions.html#from-js-to-rust

### The `#[wasm-bindgen]` macro

> The `#[wasm_bindgen]` macro is running over the syntactical (unresolved) structure of the Rust code and is then responsible for generating information that `wasm-bindgen` the _CLI tool_ later reads.

The `#[wasm_bindgen]` macro generates **executable functions** which "describe the type signature of an import or export". In detail the macro generates a implementation of the trait:
```rust
pub trait WasmDescribe { fn describe(); }
```
then the `wasm-bindgen`  command line tool runs the function to read the description
Then all these descriptor functions are pruned from the emitted wasm file.






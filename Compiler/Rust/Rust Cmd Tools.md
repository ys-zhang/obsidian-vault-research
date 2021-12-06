#Rust 
#  Cargo

**Cargo** is the build tool for [[Rust]].

- `cargo new`: create a new project
    - `cargo new some_lib --lib`: create a project with library crate
- `cargo build`: build the current project
- `cargo build --release` compile with optimizations
- `cargo doc`: build doc for all dependencies
- `cargo run`: compile and run
- `cargo clean` rm builds
- `cargo check`: checks your code to make sure it compiles but doesn’t produce an executable
- `cargo test`: run tests see [[Rust#Testing]].  Some command line options go to `cargo test`, and some go to the resulting test binary. To separate these two types of arguments, you list the arguments that go to `cargo test` followed by the separator `--` and then the ones that go to the test binary.
    - `cargo test test_name_prefix` only run the named with prefix
    - `cargo test -- --show-output`, see printed values for passing tests as well
    - `cargo test -- --test-threads=1`
    - `cargo test -- --ignored` only run ignored tests
    - `cargo test --test integration_test`  run test in an external test file/crate
- `cargo install`  install in the installation root’s _bin_ folder, default `_$HOME/.cargo/bin_`

# `rustup`

package managing tools.

- `rustup install nightly`: install nightly version of rust
- `rustup default nightly`: set nightly as default toolchain

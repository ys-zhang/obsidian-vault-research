# Questions

## CLI

1. Why set V8 flags 2 times?
     flags are set at `cli::standlone::run()`, which is called at `cli::main()`, but set again in `cli::main()` using `deno_core::set_v8_flags`
2. How module loader works?
    1. `deno::standalone::EmbeddedModuleLoader`
    2. `eszip` crate
3. File Watcher
    - function `cli/main.rs/run_with_watch()`


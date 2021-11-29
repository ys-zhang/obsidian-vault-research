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


# inline asm

for more info of the parameter, constrain and clobber see [[assembly#GNU Assembly gcc inline syntax]].

```ad-note
inline assembly on works on nightly build. (at least in Rust 2021).
```

## the old convention `llvm_asm!`

[llvm_asm - The Rust Unstable Book (rust-lang.org)](https://doc.rust-lang.org/beta/unstable-book/library-features/llvm-asm.html)

```rust
llvm_asm!(assembly template
   : output operands
   : input operands
   : clobbers
   : options
   );
```

### options
1.  `volatile` - specifying this is analogous to `__asm__ __volatile__ (...)` in gcc/clang.
2.  `alignstack` - certain instructions expect the stack to be aligned a certain way (i.e. SSE) and specifying this indicates to the compiler to insert its usual stack alignment code
3.  `intel` - use Intel syntax instead of the default AT&T.


## new style `asm!`

[asm - The Rust Unstable Book (rust-lang.org)](https://doc.rust-lang.org/beta/unstable-book/library-features/asm.html#asm)

[asm reference - The Rust Unstable Book (rust-lang.org)](https://doc.rust-lang.org/beta/unstable-book/library-features/asm.html#reference-level-explanation)

The `asm!` macro is governed by the same rules as `format!`, however, arguments to the template string is controlled by constrains.

```ad-note 
title: default syntex

On x86 platform the Intel Syntax is default. 
```

### Register operands

#### input/output constraints

Think the inline asm code as function, it has inputs and outputs.

these specifiers tells compiler constrains on register allocation/caching, etc.

- `in`: specify an expression as input parameter,
- `out`: specify an lvalue expression as output parameter 
- `inout`: both input and output, `inout(reg) x => y` takes `x` as input and `y` as output
- `const`: a literal value to pass in the template.
- `in(reg)`: tell the compiler to choose a register for us
- `out("eax")`: force to use some specific register explicitly, _need to specify the name of register literally in the template string_. 


```rust
let i: u64 = 3;
let o: u64;  // uninitialized var is lvalue
let mut a = 3;
// uses Intel syntax:
//   inst dst, src
unsafe {
  // o = i + 5;
  asm!("mov {0}, {1}",
       "add {0}, {number}",
       out(reg) o,
       in(reg) i,
       number = const 5,
  );
  
  // a += 5;
  asm!("add {0} {number}",
       inout(reg) a,
       number = const 5,
  );
}
```

#### `lateout`, `inlateout`

this constraint gives a hint to the compiler on how to allocate register for output argument.

This specifies an output that is written to memory (i.e. its register is free to be reallocate for usage) after all inputs have been consumed, which allows the output uses the same register as some input argument.


#### Clobbered register

Inline asm may modify state that is not needed as an input/output. Cases may be:
1. need some register for scratch;
2. some instructions modify state that we do not need to examine.

Compiler need to be informed of these states as it may need to save and restore these clobbered registers.


### Memory operands

```rust
#![feature(asm, llvm_asm)]
unsafe {
    asm!("fldcw [{}]",
         in(reg) &control,
         options(nostack));

    // Previously this would have been
    // written with the deprecated
    //   `llvm_asm!` like this
    llvm_asm!( "fldcw $0" 
               :
               : "m" (control) 
               :
               : "volatile"
              );
}
```


### Option


- `pure`: the asm code has no side effect
- `nomem`: the asm code does not read/write memory
- `nostack`: the asm code does not _push_ any data onto the stack.





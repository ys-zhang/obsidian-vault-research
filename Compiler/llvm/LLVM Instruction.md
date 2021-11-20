Use [IRBuilder](https://llvm.org/doxygen/classllvm_1_1IRBuilder.html) create instructions.

# instruction modifier

#### `volatile`

Certain memory accesses, such as [[#load]]’s, [[#store]]’s, and `llvm.memcpy`’s may be marked volatile. Optimizers must not change the _number_ of volatile operations or change their _order_ of execution relative to other _volatile operations_.

> “volatile” is the C/C++ volatile, which ensures that _every volatile load and store happens and is performed in the stated order_. It has nothing to do with multi-threading like in Java

#### `atomic` and ordering
[LLVM Atomic Instructions and Concurrency Guide — LLVM 13 documentation](https://llvm.org/docs/Atomics.html)

###### six levels of atomicity

1. _not atomic_: regular load/store, race condition produce undefined value;
2. _unordered_:  races produce somewhat sane results
3. #unfinished 

# Memory

#### `alloca`

Allocates memory on the _stack frame_ of the currently executing function. 

The ‘`alloca`’ instruction allocates `sizeof(<type>)*NumElements` _bytes_ of memory on the _runtime stack_, returning a _pointer_ of the appropriate type to the program.

```
; yields type addrspace(num)*:result
<result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     
```

> The allocated memory is _uninitialized_, and loading from uninitialized memory produces an _undefined_ value.

```llvm
%ptr = alloca i32                     ; yields i32*:ptr
%ptr = alloca i32, i32 4              ; yields i32*:ptr
%ptr = alloca i32, i32 4, align 1024  ; yields i32*:ptr
%ptr = alloca i32, align 1024         ; yields i32*:ptr
```

#### `store`

The ‘`store`’ instruction is used to _write to memory/pointers_.

```c++
StoreInst* IRBuilderBase::CreateStore(Value *Val, Value *Ptr, bool isVolatile=false)
```
#### `load`

The ‘`load`’ instruction is used to _read from memory/pointers_.

```llvm
%ptr = alloca i32          ; yields i32*: ptr
store i32 3, i32* %ptr     ; yields void
%val = load i32, i32* %ptr ; yields i32: val = i32 3
```


```c++
LoadInst* IRBuilderBase::CreateLoad(Type *Ty, Value *Ptr, const Twine &Name="")
 
LoadInst* IRBuilderBase::CreateLoad(Type *Ty, Value *Ptr, bool isVolatile, const Twine &Name="")
```

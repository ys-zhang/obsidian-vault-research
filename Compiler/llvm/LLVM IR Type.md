[LLVM IR Type System](https://llvm.org/docs/LangRef.html#type-system)
[The LLVM Project Blog: LLVM 3.0 type system rewrite](http://blog.llvm.org/2011/11/llvm-30-type-system-rewrite.html)

# Why need a type system

Being typed enables a number of optimizations to be performed on the intermediate representation directly, without having to do extra analyses on the side before the transformation.


# Overview

The type system consists of three major parts: 
1. primitive types (like 'double' and integer types)
2. derived types (like structs, arrays and vectors), and
3. a mechanism for handling forward declarations of types ('opaque').

The **first class types** are perhaps the most important. Values of these types are the only ones which can be produced by instructions.

![[Pasted image 20211112212619.png]]

# Primitive type

| Type         | Syntax               | Description                                                                   |
| ------------ | -------------------- | ----------------------------------------------------------------------------- |
| Void         | `void`               | The void type does not represent any value and has no size.                   |
| Integer      | `iN`                 | The number of **bits** the integer will occupy is specified by the `N` value. |
| Float        | `double`, `float`... |                                                                               |
|              | `X86_amx`, `X86_mmx` | AMX, MMX register value                                                       |
| [[#Pointer]] | `<type> *ptr`        | The pointer type is used to specify memory locations.                         |
| [[#Vector]]  |                      | For [[SIMD]]                                                                  |
| [[#Array]]   |                      |                                                                               |

## Integer 

```c++
static IntegerType* llvm::Type::getIntNTy(LLVMContext &C, unsigned N);
static IntegerType* llvm::Type::getInt1Ty(LLVMContext &C);
static IntegerType* llvm::Type::getInt8Ty(LLVMContext &C);
static IntegerType* llvm::Type::getInt16Ty(LLVMContext &C);
static IntegerType* llvm::Type::getInt32Ty(LLVMContext &C);
static IntegerType* llvm::Type::getInt64Ty(LLVMContext &C);
static IntegerType* llvm::Type::getInt128Ty(LLVMContext &C);
```


## Pointer

The pointer type is used to specify memory locations.

Pointer types may have an **optional address space attribute** defining the numbered **address space** where the pointed-to object resides. 

The default address space is number zero. The semantics of non-zero address spaces are target-specific.

Examples:	

| syntax              | meaning                                                            |
| ------------------- | ------------------------------------------------------------------ |
| `[4 x i32]*`        | A pointer to array of four `i32` values.                           |
| `i32 (i32*) *`      | A pointer to a function that takes an `i32*`, returning an `i32`.  |
| `i32 addrspace(5)*` | A pointer to an `i32` value that resides in address space 5.       |
| `ptr`               | An opaque pointer type to a value that resides in address space 0. |
| `ptr addrspace(5)`  | An opaque pointer type to a value that resides in address space 5. |
	
  
# Aggregate/Derived type


## Vector 
represents a vector of elements, used for [[SIMD]].

> vector is a first class type
  
    < <# elements> x <elementtype> >          ; Fixed-length vector
    < vscale x <# elements> x <elementtype> > ; Scalable vector

| Example              | meaning                                            |
| -------------------- | -------------------------------------------------- |
| `<4 x i32>`          | Vector of 4 32-bit integer values.                 |
| `<4 x i64*>`         | Vector of 4 pointers to 64-bit integer values.     |
| `<vscale x 4 x i32>` | Vector with a multiple of 4 32-bit integer values. | 

In general vector elements are _laid out in memory_ in the same way as [[#Array]] types.

A **bitcast** from a vector type to a scalar integer type will see the elements being packed together (without padding).  
_The order in which elements are inserted in the integer depends on endianess._

```c++
static VectorType* llvm::VectorType::get(Type* ElementType, ElementCount EC);
static VectorType* llvm::VectorType::get(Type* ElementType, unsigned 	NumElements, bool Scalable);	
```

## Array

size of array type must be constant 

    [<# elements> x <elementtype>]

| Example                 | meaning                                           |
| ----------------------- | ------------------------------------------------- |
| `[4 x i8]`              | Array of 4 8-bit integer values.                  |
| `[2 x [3 x [4 x i16]]]` | $2\times3\times4$ array of 16-bit integer values. |


## Structure
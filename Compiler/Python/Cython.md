> The fundamental nature of Cython can be summed up as follows: Cython is Python with C data types.

# Compile

The pipeline comprises two stages:
1. The first stage is handled by the `cython` compiler, which transforms Cython source into optimized and platform-independent C or C++.
2. The second stage compiles the generated C or C++ source into a shared library with a standard C or C++ compiler.

The flags passed to the C or C++ compiler ensure this shared library is a full-fledged Python module. We call this compiled module an *extension module*, and it can be imported and used as if it were written in pure Python.


## Use `setuptools`

```python
from setuptools import setup         # mustbe imported before Cython
from Cython.Build import cythonize

setup(ext_modules=cythonize('fib.pyx'))
```

The `cythonize` function in its simplest usage converts Cython source to C source code by calling the `cython` compiler.

The `cythonize` command returns a list of `setuptools.Extension` objects that the setup function knows how to turn into Python extension modules.

```bash
# `build_ext` forces to build in extenstion mode
#
# `--inplace` flag instructs setuptools to 
#   place each extension module next to its
#   respective Cython .pyx source file
python setup.py build_ext --inplace
```

# Cython language

## Dynamic Typing and Static Typing

In Cython, untyped dynamic variables behave exactly like Python variables.

To statically type variables in Cython, we use the `cdef` keyword with a type and the variable name.

```cython
cdef int i
cdef int j
cdef float k

cdef:
	int i
	int N=2000
	float dx, s=0.0
```

> The important difference between dynamic variables and static variables is that **static variables with C types have C semantics**, which changes the behavior of assignment. It also means these variables follow C coercion and casting rules.

Cython do not support `static` keyword, but support `const` keyword

| C type                              | Cython `cdef` statement         |
| ----------------------------------- | ------------------------------- |
| Pointers                            | `cdef int *p` `cdef void **buf` |
| Stack-allocated C arrays            | `cdef int arr[10]`              |
| typedefed aliased types             | `cdef size_t len`               |
| Compound types (structs and unions) | `cdef tm time_struct`           |
| Function pointers                   | `cdef void (*f)(int, double)`   |


## Automatic Type Inference in Cython

Cython also performs automatic type inference for untyped variables in function and method bodies. 

By default, Cython infers variable types only when doing so cannot change the semantics of the code.

```cython
cimport cython

def automatic_inference():
	i = 1           # object: need to comply to py semantics
	d = 2.0         # double, since the literal 2.0 is double
	c = 3+4j        # 
	r = i * d + c
	return r

@cython.infer_types(True)
def more_inference():
	i = 1           # long
	d = 2.0
	c = 3+4j
	r = i * d + c
	return r


```

By means of the `infer_types` compiler directive (see “Compiler Directives” on page 28), we can give Cython more leeway to infer types in cases that may possibly change semantics—for example, when integer addition may result in overflow

> Until now, we have used `cdef` to statically declare variables with a C type. It is also possible to use `cdef` to statically declare variables with a Python type. We can do this for the built-in types like `list`, `tuple`, and `dict`; extension types like NumPy `ndarray`; and many others.

Cython currently supports several built-in statically declarable Python types, including:
```
- type, object 
- bool 
- complex 
- basestring, str, unicode, bytes, bytearray 
- list, tuple, dict, set, frozenset 
- array 
- slice 
- date, time, datetime, timedelta, tzinfo
```


## Pointers

```cython
cimport cython

cdef double golden_ratio 
cdef double *p_double

# take reference
p_double = &golden_ratio

# dereference
golden_ratio = cython.operator.dereference(p_double)
golden_ratio = p_double[0]

# pointer to struct
#
# equiv in c
# st_t *p_st = make_struct();
# int a_doubled = p_st->a + p_st->a;

cdef st_t *p_st = make_struct()
cdef double a_doubled = p_st.a + p_st.a
```

see [[#Type Coercion and Casting]]

## Reference Counting and Static String Types

*CPython* implements *automatic memory management* via straightforward *reference counting*, with an automatic garbage collector that runs periodically to clean up unreachable reference cycles.

Cython handles all reference counting for us, ensuring a Python object (whether statically typed or dynamic) is finalized when its reference count reaches zero.

##### caveats 
```cython
b1 = b"All men are mortal." 
b2 = b"Socrates is a man." 
cdef char *buf = b1 + b2
```

> It is an issue here only because a C-level object is refer‐ ring to data that is managed by a Python object. Because the Python object owns the underlying string, the C `char *` buffer has no way to tell Python that it has another (non-Python) reference.

The `b1 + b2` expression is a temporary Python bytes object, and the assignment attempts to extract that temporary object’s char pointer using Cython’s automatic conversion rules. Because the result of the addition is a temporary object, the preceding example cannot work—the temporary result of the addition is deleted immediately after it is created, so the char buffer cannot refer to a valid Python object. Fortunately, Cython is able to catch the error and issue a compilation error.

```cython
tmp = s1 + s2 
cdef char *buf = tmp

# or

cdef bytes tmp = s1 + s2   # data are copied
cdef char *buf = tmp
```

## Type conversion

![[Pasted image 20210927131633.png]]


## functions

### Python function

`def` keyword defines a Python function. we can annotate static type in python function but the return value is always Python `object`.

### C Function

`cdef` defines a C function with C-calling semantics.

A `cdef` function’s arguments and return type are typically statically typed, and they can work with C pointer objects, structs, and other C types that cannot be automatically coerced to Python types.

>Calling the c_fact function is as efficient as calling a pure-C function, so the function call overhead is minimal. Nothing prevents us from declaring and using Python objects and dynamic variables in `cdef` functions, or accepting them as arguments. But `cdef` functions are typically used when we want to get as close to C as possible without writing C code directly.

The optional return type of a `cdef` function can be any static type we have seen, including pointers, structs, C arrays, and static Python types like list or dict. We can also have a return type of void. If the return type is omitted, then it defaults to object.

> Cython does not allow a `cdef` function to be called from external Python code.

### Combining `def` and `cdef` Functions with `cpdef`

A `cpdef` function combines features from both of the other kinds of functions and addresses many of their limitations.

A single `cpdef` function gives us these two functions automatically: we get a C-only version of the function and a Python wrapper for it, both with the same name.

Cython supports the inline keyword for `cdef` and `cpdef` functions—we simply place `inline` after the `cdef` or `cpdef` keyword:

```cython
cdef inline long c_fact(long a): 
	# ...
```

### Type Coercion and Casting

Cython provides a casting operator that is very similar to C’s casting operator, except that it replaces parentheses with angle brackets.

```cython
# int *ptr_i = (int*) v;
cdef int *ptr_i = <int*>v

# bydefault type cast if not checked
# checked type cast

cdef list cast_list = <list?>a
```

### union and struct

```cython
cdef struct mycpx:
 	float real
	float imag
 
cdef union uu:
	int a
	short b, c
	
ctypedef struct mycpx:
	float real
	float imag
	
ctypedef union uu:
	int a
	short b, c

cdef mycpx a = mycpx(3.1415, -1.0)
cdef mycpx b = mycpx(real=2.718, imag=1.618034)

cdef mycpx zz 
zz.real = 3.1415
zz.imag = -1.0

# structs can be assigned from a Python dictionary
cdef mycpx zz = {'real': 3.1415, 'imag': -1.0}


cdef enum PRIMARIES:
	RED = 1
	YELLOW = 3
	BLUE = 5

cdef enum SECONDARIES:
	ORANGE, GREEN, PURPLE
```

# NumPy and Typed Memoryview

All python builtin containers have one thing in common: *they all store references to Python objects.*

What we want is a way to represent and work with a homogeneous contiguous array, or *buffer*, of unboxed data types in Python.


```ad-note
title: new buffer protocol

The new buffer protocol’s most important feature is its ability to represent the same underlying data in different ways. It allows NumPy arrays, several Python built-in types, and Cython-level array-like objects to *share the same data without copying.*

Buffers allow us to represent contiguous or simply strided unboxed data of a single data type. NumPy arrays—the most widely used array type in Python—support the buffer protocol. [PEP-3118]
```


```plantuml

split
	-[hidden]->
	:a:A;
split again
	-[hidden]->
	:b:B;
endsplit
:char* buf;
```


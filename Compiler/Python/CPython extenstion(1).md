[Extending Python with C or C++](https://docs.python.org/3/extending/extending.html)

[How to extend NumPy — NumPy v1.21 Manual](https://numpy.org/doc/stable/user/c-info.how-to-extend.html)

[Introduction — Python 3.9.7 documentation](https://docs.python.org/3/c-api/intro.html)

# Python Object, Type and Reference Count

Almost all Python objects live on the heap: you never declare an automatic or static variable of type `PyObject`, only pointer variables of type `PyObject*` can be declared. The sole exception are the *type objects*; since these must never be deallocated, they are typically *static* `PyTypeObject` objects.

For each of the well-known types there is a macro to check whether an object is of that type; for instance, `PyList_Check(a)` is true if (and only if) the object pointed to by `a` is a Python `list`.

## Reference count

```c
// macros defined to control the ref count

Py_INCREF();  // increment ref count by 1
Py_DECREF();  // decrement ref count by 1

```

`{y_DECREF()` must check whether the reference count becomes zero and then *cause the object’s deallocator to be called*. 

The deallocator is a function pointer contained in the object’s type structure. 

The type-specific deallocator takes care of decrementing the reference counts for other objects contained in the object if this is a compound object type, such as a list, as well as performing any additional finalization that’s needed.

The only real reason to use the reference count is to prevent the object from being deallocated as long as our variable is pointing to it.

If we know that there is at least one other reference to the object that lives at least as long as our variable, there is no need to increment the reference count temporarily.

> A safe approach is to always use the generic operations (functions whose name begins with `PyObject_`, `PyNumber_`, `PySequence_` or `PyMapping_`). These operations always increment the reference count of the object they return. This leaves *the caller* with the responsibility to call `Py_DECREF()` when they are done with the result; this soon becomes second nature.

## Ownership

- _Owning a reference_ means being responsible for calling `Py_DECREF` on it when the reference is no longer needed.

- When a function passes _ownership_ of a reference on to its caller, the caller is said to receive a _new_ reference. (e.g. `PyLong_FromLong(), PyObject_GetItem(), PySequence_GetItem()`)

- When no _ownership_ is transferred, the caller is said to _borrow_ the reference. Nothing needs to be done for a borrowed reference.

- _Stealing a reference_ means that when you pass a reference to a function, that function assumes that it now owns that reference, and you are not responsible for it any longer. (e.g. `PyList_SetItem()`, `PyTuple_SetItem()`)


`Py_BuildValue()` can create most common objects from C values, directed by a _format string_.
```c
PyObject *tuple, *list;

tuple = Py_BuildValue("(iis)", 1, 2, "three");
list = Py_BuildValue("[iis]", 1, 2, "three");

/*
It is much more common to use `PyObject_SetItem()` 
	and friends with items whose references 
	you are only borrowing, like arguments that
	were passed in to the function you are writing.
*/
int set_all(PyObject *target, PyObject *item)
{
	/*
		target and item are borrowed in
	*/
	
    Py_ssize_t i, n;

    n = PyObject_Length(target);
    if (n < 0)
        return -1;
    for (i = 0; i < n; i++) {
        PyObject *index = PyLong_FromSsize_t(i);
        if (!index)
            return -1;
        if (PyObject_SetItem(target, index, item) < 0) {
            Py_DECREF(index);
            return -1;
        }
        Py_DECREF(index);
    }
    return 0;
}
```


# Exception

when a function fails, it should set an exception condition and return an error value (usually a NULL pointer).

```
      +------ first variable: E 
      v
raise E(V).with_traceback(T)  <------- third variable: T
        ^ 
        +----- second variable: V
```

1. Exceptions are stored in a static global variable inside the interpreter; if this variable is NULL no exception has occurred.
2. A second global variable stores the “associated value” of the exception (the second argument to raise).
3. A third variable contains the stack traceback in case the error originated in Python code. 

## Functions pass errors around

- `PyErr_SetString()`: Its arguments are an exception object and a C string.
- `PyErr_SetFromErrno()`: only takes an exception argument and constructs the associated value by inspection of the global variable `errno`.
- `PyErr_Occurred()`: returns the current exception object, or NULL if no exception has occurred.
- `PyErr_Clear()`: ignore an exception set by a function call that failed
- `PyErr_NoMemory()`: `malloc` fail;  All the object-creating functions (for example, `PyLong_FromLong()`) already do this, so this note is only relevant to those who call `malloc()` directly.



> Also note that, with the important exception of `PyArg_ParseTuple()` and friends, functions that return an integer status usually return a positive value or zero for success and -1 for failure, like Unix system calls.

> be careful to clean up garbage (by making `Py_XDECREF()` or `Py_DECREF()` calls for objects you have already created) when you return an error indicator!

> When a function `f` that calls another function `g` detects that the latter fails, `f` should itself return an error value (usually NULL or -1). It should not call one of the `PyErr_()` functions — one has already been called by `g`.
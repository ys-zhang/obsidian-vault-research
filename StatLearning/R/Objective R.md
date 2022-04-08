# Base Object

base object's are supported in **R**'s runtime implementation, i.e., in **C** level.

for each **R object** in C level a info header is used to store meta data about the object as a **Base Object**.
```c
// src/include/RInternal.h

#define NAMED_BITS 16
struct sxpinfo_struct {
    SEXPTYPE type      :  5;  /* discussed above */
    unsigned int scalar:  1;  /* is this a numeric vector of length 1? */
    unsigned int obj   :  1;  /* is this an object with a class attribute? */
    unsigned int alt   :  1;  /* is this an `ALTREP` object? */
    unsigned int gp    : 16;  /* general purpose, see below */
    unsigned int mark  :  1;  /* mark object as ‘in use’ in GC */
    unsigned int debug :  1;
    unsigned int trace :  1;
    unsigned int spare :  1;  /* debug once and with reference counting */
    unsigned int gcgen :  1;  /* generation for GC */
    unsigned int gccls :  3;  /* class of node for GC */
    unsigned int named : NAMED_BITS; /* used to control copying */
    unsigned int extra : 32 - NAMED_BITS;
}; /*		    Tot: 64 */
```

the field `type` with 5 bits indicates the **base type** of the object

```c
// src/include/RInternal.h

typedef enum {
    NILSXP	= 0,	/* nil = NULL */
    SYMSXP	= 1,	/* symbols */
    LISTSXP	= 2,	/* lists of dotted pairs */
    CLOSXP	= 3,	/* closures */
    ENVSXP	= 4,	/* environments */
    PROMSXP	= 5,	/* promises: [un]evaluated closure arguments */
    LANGSXP	= 6,	/* language constructs (special lists) */
    SPECIALSXP	= 7,	/* special forms */
    BUILTINSXP	= 8,	/* builtin non-special forms */
    CHARSXP	= 9,	/* "scalar" string type (internal only)*/
    LGLSXP	= 10,	/* logical vectors */
    INTSXP	= 13,	/* integer vectors */
    REALSXP	= 14,	/* real variables */
    CPLXSXP	= 15,	/* complex variables */
    STRSXP	= 16,	/* string vectors */
    DOTSXP	= 17,	/* dot-dot-dot object */
    ANYSXP	= 18,	/* make "any" args work */
    VECSXP	= 19,	/* generic vectors */
    EXPRSXP	= 20,	/* expressions vectors */
    BCODESXP	= 21,	/* byte code */
    EXTPTRSXP	= 22,	/* external pointer */
    WEAKREFSXP	= 23,	/* weak reference */
    RAWSXP	= 24,	/* raw bytes */
    S4SXP	= 25,	/* S4 non-vector */

    NEWSXP      = 30,   /* fresh node creaed in new page */
    FREESXP     = 31,   /* node released by GC */

    FUNSXP	= 99	/* Closure or Builtin */
} SEXPTYPE;
```

# S3

**S3** supports ad-hot polymorphism through the following mechanism:
1. **object attribute**
2. **run time dispatching**
3. **generic function**


## `class` attribute


Many _R objects_ have a **class** attribute, a character vector giving the _names of the classes_ from which the object **inherits**.

If the object does not have a `class` attribute, it has an **implicit class**:
- `matrix`, `array`, `function`, `numeric`, `NULL`
- `typeof(x)`, `mode(x)`

## runtime dispatch

- When a **generic function** `fun` is applied to an object with **class attribute** `c("first", "second")`
    1. the system searches for a function called `fun.first` and, if it finds it, applies it to the object. 
    2. If no such function is found, a function called `fun.second` is tried. 
    3. If no class name produces a suitable function, the function `fun.default` is used (if it exists).
- If there is **no class attribute**, 
    1. the _implicit class_ is tried
    2. then the _default method_.


# S4




# R6



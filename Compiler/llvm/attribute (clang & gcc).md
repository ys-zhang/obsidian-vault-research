[Attributes in Clang — Clang 13 documentation (llvm.org)](https://clang.llvm.org/docs/AttributeReference.html)

This is part of [Clang Language Extensions](https://clang.llvm.org/docs/LanguageExtensions.html) or [C Extensions (GCC)](https://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html).

# Syntax: How to assign attributes
[Attribute Syntax (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html#Attribute-Syntax)

> attribute specifier annotates what preceding it.

## _attribute specifier_

     __attribute__ ((attribute-list))
     
Examples

```
__attribute__ (())                     // Empty attributes are ignored
__attribute__ ((__attr_name__))        // attribute name

// attribute name with a parameter list
__attribute__ ((__attr_name__(param1, param2)))   

```

## Variable attributes

```c
int x __attribute__ ((aligned (16))) = 0;
```

## Label attributes

```c
asm goto ("some asm" : : : : NoError); 
/* This branch (the fall-through from the asm) is less commonly used */ 
ErrorHandling: 
    __attribute__((cold, unused)); /* Semi-colon is required here */ 
    printf("error\n"); 
    return 0;

NoError:
    printf("no error\n");
    return 1;
```

| Attribute | meaning                                               |
| --------- | ----------------------------------------------------- |
| unused    | program-generated code that may contain unused labels |
| hot       | path more likely pass here                            |
| cold      | path unlikely pass here                               |


## Enumerate attributes

```c
enum E {
  oldval __attribute__((deprecated)),
  newval
};

int fn (void)
{
  return oldval;
}
```

| Attribute   | meaning                                       |
| ----------- | --------------------------------------------- |
| deprecated  | this case is to be removed in the future      |
| unavailable | results in an error if the enumerator is used |


## Statement attributes

```c
switch (cond)
  {
  case 1:
    bar (1);
    __attribute__((fallthrough));
  case 2:
    …
  }
```



# Attributes

`weak`:  The `weak` attribute causes a declaration of an external symbol to be emitted as a [[weak symbol (link)]] rather than a global. This is primarily useful in _defining library functions that can be overridden in user code_, though it can also be used with non-function declarations.
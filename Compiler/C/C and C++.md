

# C

## Lexical Analysis

- Expression
	- Type
	- Value Category
		- lvalue
			- modifiable lvalue: not an array, and not `const`-qualified and have no substructure that is `const`-qualified.
		- non-lvalue/rvalue
			- all operators not specified to return lvalues
		- function designator

## Type qualifier

> Each individual type in the C type system has several _qualified_ versions of that type, corresponding to one, two, or all three of the `const`, `volatile`, and, for pointers to object types, `restrict` qualifiers.

##### The `const` type qualifier

> Objects declared with `const`-qualified types may be placed in **read-only memory** by the compiler, and if the address of a `const object` is never taken in a program, it may not be stored at all.


# string

> It's defined by the ISO C standard, adjacent string literals are combined into a single one.

# C++
## Type

### String

#### quotes
- `'s'` is a `char`
- `"s"` is a ***c-string***.
- `string` class
```C++
#include <string>

char* apple = "apple";
std::string fruit = apple;

```

### 类型转换


```C++
static_cast<Type>(Expression);
	
// The const_cast is used to cast away constantness.
const_cast<Type>(Expression);  

// dynamic_cast is used for safe downcasting 
//   from one type to a descendent type in an inheritance hierarchy.
dynamic_cast<Type>(Expression);

// The reinterpret_cast is an implementation-dependent cast
reinterpret_cast<Type>(Expression);
```


```C++
// new form of static cast
int m,n
double ans = n/static_cast<double>(m);

// older form of static cast
ans = n / double(m);  
ans = n / (double) m;
```

see [[Operator Precedence#C and CPP]].


# stdlib
## cin/cout

```c++
#include <iostream>
using namespace std;

string day1 = "Monday", day2="Tuesday";  
cout << day1 + day2 << endl;

cout << "Enter the number of dragons\n"  
	 << "followed by the number of trolls.\n";  
cin >> dragons >> trolls;

```

# References

[MicrosoftDocs/cpp-docs: C++ Documentation (github.com)](https://github.com/MicrosoftDocs/cpp-docs)
[Microsoft C/C++ 文档 | Microsoft Docs](https://docs.microsoft.com/zh-cn/cpp/?view=msvc-160)
[C++ 文档 - 入门、教程、参考 | Microsoft Docs](https://docs.microsoft.com/zh-cn/cpp/cpp/?view=msvc-160)



# snippets

```c
#define FLIP_BIT(_ar, _b) do { \
    u8* _arf = (u8*)(_ar); \
    u32 _bf = (_b); \
    _arf[(_bf) >> 3] ^= (128 >> ((_bf) & 7)); \
   } while (0)
```
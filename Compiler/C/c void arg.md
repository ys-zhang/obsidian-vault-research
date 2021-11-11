```c
// correct way to say "no parameters" in C,
//   and it also works in C++
void foo(void);  

// in C it means "could take any number of parameters of unknown types"
// in C++ it means the same as `foo(void)`.
void foo();
```
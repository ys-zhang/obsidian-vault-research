- [ ] why define a macro as an alias of a function 
```c: AFL/alloc-inl.h
#define ck_strdup  DFL_ck_strdup
static inline u8* DFL_ck_strdup(u8* str) {...}
```
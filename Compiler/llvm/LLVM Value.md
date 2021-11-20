# `llvm::User`

## `llvm::Constant`

### `llvm::UndefValue`

[nondot.org/sabre/LLVMNotes/UndefinedValue.txt](http://nondot.org/sabre/LLVMNotes/UndefinedValue.txt)

**semantics**: when assign a variable with `UndefValue` represents it is only declared but not defined.

```c
int test() {
  int Y;
  return Y;
}
```
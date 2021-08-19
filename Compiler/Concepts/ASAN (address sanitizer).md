[AddressSanitizer · google/sanitizers Wiki (github.com)](https://github.com/google/sanitizers/wiki/AddressSanitizer)
[Instrumentation Options (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gcc/Instrumentation-Options.html#index-fsanitize_003daddress)

Memory Error detector.

checks for
- use after free
- heap buffer overflow
- stack buffer overflow
- global buffer overflow
- use after return
- use after scope
``` c
// RUN: clang -O -g -fsanitize=address -fsanitize-address-use-after-scope \
//    use-after-scope.cpp -o /tmp/use-after-scope
// RUN: /tmp/use-after-scope

// Check can be disabled in run-time:
// RUN: ASAN_OPTIONS=detect_stack_use_after_scope=0 /tmp/use-after-scope

volatile int *p = 0;

int main() {
  {
    int x = 0;
    p = &x;
  }
  *p = 5;  // <------------ HERE
  return 0;
}
```
- initialization order bugs
- memory leaks




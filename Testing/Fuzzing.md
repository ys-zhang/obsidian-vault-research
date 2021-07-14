#program-test

# Fuzzing Architecture

## Basic Fuzzing
> The goal of this project is to evaluate the robustness of various UNIX utility programs, given an unpredictable input stream. [...] 
> First, you will build a **fuzz generator**. This is a program that will output a random character stream. 
> Second, you will take the **fuzz generator** and use it to attack as many UNIX utilities as possible, with the goal of trying to break them.
> from _Bart Miller_

```go
// Fuzzer output a random character stream
type Fuzzer interface {
	Fuzz() []byte
	// A `Fuzzer` can be paired with a `Runner`,
	//    which takes the fuzzed strings as input.
	Run(Runner) (status string, Outcome)
}

type Runner = func([]byte) (status string, Outcome) 

type Outcome int
const (
	PASS       Outcome = 0
	FAIL       Outcome = 1
	UNRESOLVED Outcome = 2
)
```

## Coverage



Give a *specification* and a *program* implements the specification, we can try two kinds of testing approaches:
- [[Black-box testing]]: test based on *specification/document*
- [[White-box testing]]: test based on *implementation*

>How do we measure the effectiveness of these tests? One way would be to check the number (and seriousness) of bugs found; but if bugs are scarce, we need a _**proxy for the likelihood of a test to uncover a bug**_

>If a statement in the code is not executed during testing, for instance, this means that an error in this statement cannot be triggered either.

Two types of coverage criteria:
 -  _**Statement coverage**_ – each statement in the code must be executed by at least one test input.
 -  _**Branch coverage**_ – each branch in the code must be taken by at least one test input



# Lexical Fuzzing



# Some other Issues
## Memory Access Checking
>To catch problematic memory accesses during testing, one can run C programs in special _memory-checking_ environments; at runtime, these check for each and every memory operation whether it accesses valid and initialized memory.
- [LLVM Address Sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)
```
clang -fsanitizer=address -g -o program program.c
```

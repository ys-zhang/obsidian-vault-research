#program-test  #fuzzing
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
- [[blackbox testing]]: test based on *specification/document*
- [[whitebox testing]]: test based on *implementation*

>How do we measure the effectiveness of these tests? One way would be to check the number (and seriousness) of bugs found; but if bugs are scarce, we need a _**proxy for the likelihood of a test to uncover a bug**_

>If a statement in the code is not executed during testing, for instance, this means that an error in this statement cannot be triggered either.

Two types of coverage criteria:
 -  _**Statement coverage**_ – each statement in the code must be executed by at least one test input.
 -  _**Branch coverage**_ – each branch in the code must be taken by at least one test input

The function call graph can be retrieved by `tracing` libraries.
Compilers are also generally implemented coverage utilities `cc --coverage`


# Lexical Fuzzing

## Mutation-Based Fuzzing

[[American Fuzzy Lop (AFL)]] is a fuzzing tool.

IDEA: start with a given _valid_ input, and then to subsequently _mutate_ it.

The problem here is how to design mutate operators:
- inserting a (random) character
- deleting a character
- flipping a bit
- ... 


### How can we leverage coverage to guide test generation?
-   For AFL, "success" means _finding a new path through the program execution_.
-   Randomly generated inputs are frequently invalid – and thus exercise mostly input processing functionality.
-   Mutations from existing valid inputs have much higher chances to be valid, and thus to exercise functionality beyond input processing.

## Graybox Fuzzing

[[greybox fuzzing]] uses lightweight instrumentation to glean some information about the (branch) coverage of a generated input. If a generated input increases coverage, it is added to the seed corpus for further fuzzing. 

The instrumentation is usually done at compile-time, i.e., when the program source code is compiled to an executable binary. However, it is possible to run AFL on uninstrumented binaries using tools such as a virtual machine (e.g., [QEMU](https://github.com/mirrorer/afl/blob/master/qemu_mode)) or a dynamic instrumentation tool (e.g., [Intel PinTool](https://github.com/vanhauser-thc/afl-pin)).

## Directed Greybox Fuzzing
[[Directed greybox fuzzing]]

# Some other Issues
## Memory Access Checking
>To catch problematic memory accesses during testing, one can run C programs in special _memory-checking_ environments; at runtime, these check for each and every memory operation whether it accesses valid and initialized memory.
- [LLVM Address Sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)
```
clang -fsanitizer=address -g -o program program.c
```

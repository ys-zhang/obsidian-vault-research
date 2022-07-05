IR for golang  #asm #golang

# Symbols

**Instructions, registers, and assembler directives are always in UPPER CASE** to remind you that assembly programming is a fraught endeavor. (Exception: the `g` register renaming on ARM.)


In Go object files and binaries, the full name of a symbol is the package path followed by a period and the symbol name: `fmt.Printf` or `math/rand.Int`. 
Because the assembler's parser treats period and slash as punctuation, those strings cannot be used directly as identifier names. 
Instead, the assembler allows the middle dot character `U+00B7` and the division slash `U+2215` in identifiers and rewrites them to plain period and slash. (`fmt·Printf` and `math∕rand·Int`)

Most hand-written assembly files do not include the full package path in symbol names, because the linker inserts the package path of the current object file at the beginning of any name starting with a period: in an assembly source file within the math/rand package implementation, the package's Int function can be referred to as `·Int`. This convention avoids the need to hard-code a package's import path in its own source code, making it easier to move the code from one location to another.


## pseudo register & addressing

-   `FP`: Frame pointer: arguments and locals.
-   `PC`: Program counter: jumps and branches.
-   `SB`: Static base pointer: global symbols.
-   `SP`: Stack pointer: the highest address within the local stack frame.

All user-defined symbols are written as offsets to the pseudo-registers `FP` (arguments and locals) and `SB` (globals).


```
	-----------------
	|    caller     |  <------- RET address
	| ------------- |
	| FRAME POINTER |  <------- FP
	| function args |
	| ------------- |  <------- stack base
	| locals vars   |
	| ------------- |  <------- SP
```


### `SB`(static base pointer): global symbols

The `SB` pseudo-register can be thought of as the origin of memory, so the symbol `foo(SB)` is the name `foo` as an address in memory. 

This form is used to name global functions and data. Adding `<>` to the name, as in `foo<>(SB)`, makes the name visible only in the current source file, like a top-level `static` declaration in a C file.

Adding an offset to the name refers to that offset from the symbol's address, so `foo+4(SB)` is four bytes past the start of `foo`.


### `FP`(frame pointer): function arguments

The `FP` pseudo-register is a *virtual frame pointer used to refer to function arguments*. The compilers maintain a virtual frame pointer and refer to the arguments on the stack as offsets from that pseudo-register.

Thus `0(FP)` is the first argument to the function, `8(FP)` is the second (on a 64-bit machine), and so on. 
However, when referring to a function argument this way, it is necessary to place a name at the beginning, as in `first_arg+0(FP)` and `second_arg+8(FP)`. (The meaning of the offset—offset from the frame pointer—distinct from its use with `SB`, where it is an offset from the symbol.) 

The assembler enforces this convention, rejecting plain `0(FP)` and `8(FP)`. 
The actual name is semantically irrelevant but should be used to document the argument's name. It is worth stressing that `FP` is always a pseudo-register, not a hardware register, even on architectures with a hardware frame pointer.


### `SP`(stack pointer): local variables


The `SP` pseudo-register is a virtual stack pointer used to refer to frame-local variables and the arguments being prepared for function calls. It points to the highest address within the local stack frame, so references should use negative offsets in the range `[−framesize, 0)`: `x-8(SP)`, `y-4(SP)`, and so on.

	symbol-name offset (pseudo pointer)
	
## Jump and label

Branches and direct jumps are always written as offsets to the `PC`, or as jumps to **labels**.

*Each label is visible only within the function in which it is defined.*

It is therefore permitted for multiple functions in a file to define and use the same label names. Direct jumps and call instructions can target text symbols, such as `name(SB)`, but not offsets from symbols, such as `name+4(SB)`.



# Directives


## TEXT derivative 
```asm
#                             flag   frame size
#                              |      |
#                              v      v
TEXT runtime·profileloop(SB),NOSPLIT,$8
	MOVQ	$runtime·profileloop1(SB), CX
	MOVQ	CX, 0(SP)
	CALL	runtime·externalthreadhandler(SB)
	RET
```

- The `TEXT` directive declares the symbol `runtime·profileloop` and the instructions that follow form the body of the function.
- The last instruction in a `TEXT` block must be some sort of jump, usually a `RET`
- The frame size `$24-8` states that the function has a 24-byte frame and is called with 8 bytes of argument, which live on the caller's frame. If `NOSPLIT` is not specified for the `TEXT`, the argument size must be provided.


## DATA and GLOBAL derivative

Global data symbols are defined by a sequence of initializing `DATA` directives followed by a `GLOBL` directive. 

Each `DATA` directive initializes a section of the corresponding memory.

	DATA	symbol+offset(SB)/width, value

which **initializes** the *symbol* memory at the given *offset* and *width* with the given *value*.

```asm
DATA divtab<>+0x00(SB)/4, $0xf4f8fcff
DATA divtab<>+0x04(SB)/4, $0xe6eaedf0
...
DATA divtab<>+0x3c(SB)/4, $0x81828384
GLOBL divtab<>(SB), RODATA, $64

GLOBL runtime·tlsoffset(SB), NOPTR, $4
```

declares and initializes `divtab<>`, a read-only 64-byte table of 4-byte integer values, and declares `runtime·tlsoffset`, a 4-byte, implicitly zeroed variable that contains no pointers.

## arguments to the directives

-   `NOPROF` = 1  (For `TEXT` items.) Don't profile the marked function. This flag is deprecated.
-   `DUPOK` = 2  It is legal to have multiple instances of this symbol in a single binary. The linker will choose one of the duplicates to use.
-   `NOSPLIT` = 4  (For `TEXT` items.) Don't insert the preamble to check if the [[stack split | stack must be split]]. The frame for the routine, plus anything it calls, must fit in the spare space remaining in the current stack segment. Used to protect routines such as the stack splitting code itself.
-   `RODATA` = 8   For `DATA` and `GLOBL` items.) Put this data in a read-only section.
-   `NOPTR` = 16  (For `DATA` and `GLOBL` items.) This data contains no pointers and therefore does not need to be scanned by the garbage collector.
-   `WRAPPER` = 32  (For `TEXT` items.) This is a wrapper function and should not count as disabling `recover`.
-   `NEEDCTXT` = 64   (For `TEXT` items.) This function is a closure so it uses its incoming context register.
-   `LOCAL` = 128  This symbol is local to the dynamic shared object.
-   `TLSBSS` = 256   (For `DATA` and `GLOBL` items.) Put this data in thread local storage.
-   `NOFRAME` = 512  
    (For `TEXT` items.) Do not insert instructions to allocate a stack frame and save/restore the return address, even if this is not a leaf function. Only valid on functions that declare a frame size of 0.
-   `TOPFRAME` = 2048  
    (For `TEXT` items.) Function is the outermost frame of the call stack. Traceback should stop at this function.
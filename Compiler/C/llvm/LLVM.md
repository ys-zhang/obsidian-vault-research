[LLVM Intro Youtube](https://www.youtube.com/watch?v=J5xExRGaIIY)
[GitHub - antonio-morales/Fuzzing101](https://github.com/antonio-morales/Fuzzing101)

# Compile process

	file.cpp -> Clang AST -> LLVM IR -> Selection DAG -> LLVM MIR -> Machine Code
	clang: file.cpp -> LLVM IR
	llc:   LLVM IR  -> LLVM MIR -> Machine Code

## C -> LLVM IR
```bash
clang -O3 -Xclang -disable-llvm-passes   \ # prepare for O3 but don't run it
	     -S -emit-llvm code.c -o code.ll        \ # emit LLVM IR for code.c into code.ll;  -S: ouput human readable IR
opt -S -mem2reg -instnamer code.ll -o code_before_opt.ll   \ # slite clean up; -mem2reg: use register instead of stack; -instnamer: give name to instructions
```


# LLVM Tutorial

[original blog](https://mukulrathi.com/create-your-own-programming-language/llvm-ir-cpp-api-tutorial/)

## Understanding LLVM IR

> LLVM IR looks like assembly with types

1. unlimited _virtual registers_: `%0, %1, %2 ...`, register names have prefix `%`;
2. _registers_ and _instructions_ are strongly typed: `int8`, `int32`, `int1`
3. **static single assignment (SSA)** [[SSA form]]

```llvm
define i32 @factorial(i32) {
entry:
  %eq = icmp eq i32 %0, 0                         // n == 0
  br i1 %eq, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  %sub = sub i32 %0, 1                            // n - 1
  %2 = call i32 @factorial(i32 %sub)              // factorial(n-1)
  %mult = mul i32 %0, %2                          // n * factorial(n-1)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 1, %then ], [ %mult, %else ]
  ret i32 %iftmp
}
```

![[Pasted image 20220928131251.png]]
![[Pasted image 20220928131335.png]]

![[Pasted image 20220928131713.png]]
![[Pasted image 20220928132044.png]]

>[!NOTE] PHI instruction
> The `phi` instruction represents **conditional assignment**: assigning different values depending on which _preceding basic block_ we’ve just come from.
>
> It is of the form `phi type [val1, predecessor1], [val2, predecessor2], ...` 
>
> In the example above, we set `%iftmp` to 1 if we’ve come from the `then` block, and `%mult` if we’ve come from the `else` block.


## LLVM API

The C++ LLVM api is organised to match LLVM's IR structure.
```
[module]
  - [function]
    - [basic block]
```

`Value` is the base class for any IR related concepts, including

- `Module`, `Function` and `BasicBlock`;
- `Instruction` and result of intermediate computation ( `Register` )

The `Expression` type represents source language's expression. 





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

# LLVM-IR

## hierarchy
- llvm::module
	- llvm::global_variable
	- llvm::function:   (declarations and definitions)
		- llvm::basic_block
			- llvm::instr
	- constant_literals
	- value: almost anything, mostly constants and instructions

## naming
| symbol type                | naming rule      | 
| -------------------------- | ---------------- |
| global                     | start with `@`   |
| local                      | start with `%`   |
| basic block (when used)    | start with `%`   |
| basic block (when defined) | **end with** `:` |



# Info

[LLVM’s Analysis and Transform Passes](https://llvm.org/docs/Passes.html)

[Getting Started with the LLVM System](https://llvm.org/docs/GettingStarted.html)

[LLVM Reference](https://llvm.org/docs/Reference.html)

[LLVM Programmer’s Manual — C++ API](https://llvm.org/docs/ProgrammersManual.html)
[LLVM Coding Standards](https://llvm.org/docs/CodingStandards.html)

file extension name:
- `ll`: human readable LLVM IR
- `bc`: byte code LLVM IR

## Source code
[GitHub - llvm/llvm-project: The LLVM Project is a collection of modular and reusable compiler and toolchain technologies. Note: the repository does not accept github pull requests at this moment. Please submit your patches at http://reviews.llvm.org.](https://github.com/llvm/llvm-project)

	Each sub folder of the git repository of a separate sub-project. 
	
  
  ## Pass
  
  [Writing an LLVM Pass](https://llvm.org/docs/WritingAnLLVMPass.html)
  
  
## Command line tools
[LLVM Command Guide](https://llvm.org/docs/CommandGuide/index.html)
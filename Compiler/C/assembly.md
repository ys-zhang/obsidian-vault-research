[AT&T Assembly Syntax | Sig9 (ucdavis.edu)](https://csiflabs.cs.ucdavis.edu/~ssdavis/50/att-syntax.htm)

The GNU Assembler uses AT&T by default, here we use the AT&T syntax.

# AT&T
## General form

	mnemonic	source, destination
	
## Registers

`%reg-name`: `%ax`, `bx`

## Literal

`$value`: `$100`

```asm
mov	$100,	%bx    # move 100 to %bx
mov	$A,	%al	         # move 'A' to %al
```

## Memory Addressing

	segment-override:signed-offset(base,index,scale)
	
for more see [[ELF (Executable and Linkable Format)#SECTION]]



# GNU Assembly (gcc inline syntax)

[GCC-Inline-Assembly-HOWTO](https://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html)


for llvm see [LLVM Language Reference Manual](https://llvm.org/docs/LangRef.html#inline-assembler-expressions)


```c
/* moves the contents of ecx to eax */
asm("movl %ecx %eax"); 
/*moves the byte from bh to the memory pointed by eax */
__asm__("movb %bh (%eax)"); 


// instructions are seperated by `\n\t`
__asm__ ("movl %eax, %ebx\n\t"
         "movl $56, %esi\n\t"
         "movl %ecx, $label(%edx,%ebx,$4)\n\t"
         "movb %ah, (%ebx)");

/*
 * We need to inform the compiler that some of the
 *   register's value is changed or else the compiler will
 *   generate instructions uses wrong value.
 *
 * WE TREAT THE INLINE ASM as a FUNCTION
 */
asm (  assembler template 
     : output operands                  /* optional */
     : input operands                   /* optional */
     : list of clobbered registers      /* optional */
     );

// an example
int a=10, b;
asm ("movl %1, %%eax; \n\t" 
     "movl %%eax, %0;"
     :"=r" (b)   // output operand refer to %0 
     :"r"  (a)   // input operant refer to %1 
     // clobbered register: %eax get altered 
     :"%eax" );
```



####  Operand
syntax:

    "constraints" (C-expr)
  
  - _constraint_ decide addressing mode for operand
  - _operand_ is referenced by _numbers_; `%0`, `%1` ...


> [!NOTE] Output operand expr must be `lvalue`
> 
> If the output expression cannot be _directly addressed_ (for example, it is a bit-field), our constraint must allow a _register_. In that case, `GCC` will use the register as the output of the `asm`, and then store that register contents into the output.


> [!WARNING]
> 
> As stated above, ordinary output operands must be write-only; _GCC will assume that the values in these operands before the instruction are dead and need not be generated_. see [[SSA form]]



```c
// multily x by 5
//   constraint "r" specifies the compiler 
//   to choose a register for the expr
asm(  "leal (%1, %1, 4) %0"
    : "=r" (y) // any register may be chosen
    : "r"  (x) // any register may be chosen
   );

// specify the same register
asm(  "leal (%0, %0, 4) %0"
    : "=r" (y) // any register may be chosen
    : "0"  (x) // use register %0
   // no need clobber list
   // as compiler do the allocation
   );  

// choose register ourselves
asm(  "leal (%%ecx, %%ecx, 4), %%ecx"
    : "=c" (x)  // use register %ecx
    : "c"  (x)
    // no need to put %ecx on clobber list
    // as compiler knows it goes to x
    );
```


#### Clobber List

> This is to inform gcc that we will _use and modify_ them _ourselves_. So gcc will not assume that the values it loads into these registers will be valid.

> [!NOTE]
> If the instructions use any other registers, implicitly or explicitly (and the registers are _not present_ either in _input_ or in the _output_ constraint list), then those registers have to be specified in the _clobbered_ list.


- If our instruction can alter the **condition code register**, we have to add "**cc**" to the list of clobbered registers.
- add "**memory**" to the list of clobbered registers will force the compiler to *not keep memory values cached in registers* across the assembler instruction.
- Add the **volatile** keyword if the memory affected is not listed in the inputs or outputs of the asm.

#### Constraints

When operands are specified using this constraint, they get stored in [[register#GPR general purposed registers | GPR]].

###### Register operand constraint (`r`)

| r   | Registers      |
| --- | -------------- |
| a   | %eax, %ax, %al |
| b   | %ebx, %bx, %bl |
| c   | %ecx, %cx, %cl |
| d   | %edx, %dx, %dl |
| S   | %esi, %si      |
| D   | %edi, %di      | 

###### Matching(Digit) constraints

    asm ("incl %0" :"=a"(var):"0"(var));
    
###### Memory operand constraint (`m`)

When the operands are in the memory, any operations performed on them will occur directly in the memory location, as opposed to register constraints, which first store the value in a register to be modified and then write it back to the memory location.

#### Constraint Modifiers

1.  "=" : Means that this operand is _write-only_ for this instruction; the previous value is discarded and replaced by output data.
2.  "&" : Means that this operand is an _early clobber operand_, which is modified before the instruction is finished using the input operands. Therefore, this operand may not lie in a register that is used as an input operand or as part of any memory address.


# Rust inline asm

see [[unsafe rust#inline asm]]

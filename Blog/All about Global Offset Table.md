#asm  
#linker
#blog

copied from [All about Global Offset Table | MaskRay](https://maskray.me/blog/2021-08-29-all-about-global-offset-table)

# All about Global Offset Table

## Symbol address

In an executable or shared object (i.e. _a component in ELF_), a text section, which contains the code, may need to refer a symbol (e.g. call a function or use a variable). 

These referred symbol's _absolute virtual address_ is needed for accessing their values. 

The address may be:
1. a link-time constant
2. the load base plus a link-time constant
3. dependent on runtime computation by `ld.so`


### Link-time constant

For the first case, this component must be a **position-dependent executable**: _a link-time address equals its virtual address at run-time_. 

The text section can hold the absolute virtual address directly or use a PC-relative addressing.

```
# AT&T: op src, dst 

# `var` is a symbol, the linker will substitude
#    with the address it refers
movl var, %eax       
```


### Load base plus constant

For the second case, this component is either a **position-independent executable** or a **shared object**. 

> The difference between the link-time addresses of two symbols equals their virtual address difference at run-time. 

The first byte of the program image, the ELF header, is loaded at the load base. 

The text section can get the current program counter, then add the distance from PC to the symbol (PC-relative address), to compute the run-time virtual address.

```
# x86_64  
movl    var(%rip), %eax      # R_X86_64_PC32
```


### Runtime computation by ld.so

For the third case, we need help from the **runtime loader (abbreviated as ld.so)**. 
The linker emits a dynamic relocation to let _the runtime loader perform a symbol lookup to determine the associated symbol value at runtime_.

The symbol is either potentially defined in another component or is a `STT_GNU_IFUNC` symbol. See [GNU indirect functions](https://willnewton.name/2013/07/02/using-gnu-indirect-functions/) for `STT_GNU_IFUNC`.

If the text section holds the address which is relocated by the dynamic relocation, this is called **text relocations**.

More commonly, the address is stored in the **Global Offset Table (abbreviated as GOT)**. 
_The compiler emits code which uses position-independent addressing to extract the absolute virtual address from GOT_. 
The relocations (e.g. `R_AARCH64_ADR_GOT_PAGE`, `R_X86_64_REX_GOTPCRELX`) are called **GOT-generating**. 
The linker will create entries in the Global Offset Table.

```
# x86_64  
# var@GOTPCREL = &got(var) - rip  
# var@GOTPCREL(%rip): load the GOT entry  
movq    var@GOTPCREL(%rip), %rax  # R_X86_64_REX_GOTPCRELX  
movl    (%rax), %eax
```


## Global Offset Table

The Global Offset Table (usually consists of `.got` and `.got.plt`) holds the symbol addresses which are referenced by text sections. _The table holds link-time constant entries and entries which are relocated by a dynamic relocation_.

`.got.plt` holds symbol addresses used by PLT entries. `.got` holds everything else.

>[!NOTE] Why do we need a GOT entry for a link-time constant? 
>Well, at compile time it is probably undecided whether the entry may resolve to another component. The compiler may emit a GOT-generating relocation and use an indirection in a conservative manner. At link time the linker may find that the value is a constant.

### Life of a .got entry

#### Compiler behaviour

##### Defined symbols

Defined symbols generally belong to the first and second cases. However, on ELF, a non-local default visibility symbol in a shared object is preemptible by default. For `-fpic` code, the third case is used: since such a definition _may be interposed_ by another definition at runtime, the compiler conservatively uses GOT indirection.

```c
int var;  
int foo() { return var; }
```

```
# -fno-pic or -fpie
movl    var(%rip), %eax  # R_X86_64_PC32

# -fpic
movq    var@GOTPCREL(%rip), %rax  # R_X86_64_REX_GOTPCRELX
movl    (%rax), %eax
```
Using the C/C++ internal linkage (`static`, unnamed namespace) or protected/hidden visibility can avoid the indirection for `-fpic`.


##### Undefined symbols

If the symbol has the default visibility, the definition may be in a different component. For position independent code (`-fpie` and `-fpic`), the compiler uses GOT indirection conservatively.
```c
extern int ext_var;  
int foo() { return ext_var; }
```

```
movq    ext_var@GOTPCREL(%rip), %rax  
movl    (%rax), %eax
```

For position dependent code (`-fno-pic`), traditionally the compiler optimizes for statically linked executables and uses direct addressing (usually absolute relocations). How does it work if the symbol is actually defined in a shared object? To avoid text relocations, there are copy relocations and canonical PLT entries. It essentially changes the third case (symbol lookup) to the first two cases. See [Copy relocations, canonical PLT entries and protected visibility](https://maskray.me/blog/2021-01-09-copy-relocations-canonical-plt-entries-and-protected) for details.

If the symbol has a non-default visibility, the definition must be defined in the component. The compiler can safely assume the address is either a link-time constant or the load base plus a constant.

```c
__attribute__((visibility("hidden")))  
extern int ext_hidden_var;  
int foo() { return ext_hidden_var; }
```

```
movl ext_hidden_var(%rip), %eax
```


#### Linker behaviour

A **GOT-generating relocation** references a symbol. When the linker sees such a referenced symbol for the first time, it reserves an entry in GOT. For subsequent GOT-generating relocations referencing the same symbol, the linker just reuses this entry. The address of the GOT entry is insignificant.

Technically the linker can use multiple entries for one symbol. It just wastes space for the majority of cases, but some awful ABIs do use multi-GOT, e.g. mips and ppc32.

The entry needs a dynamic relocation or is a link-time constant.




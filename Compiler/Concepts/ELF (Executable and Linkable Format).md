
[[ELF_Format.pdf]] is a full text about this topic.

# Executable and Linkable Format

![[Pasted image 20210911115618.png]]

- **linkable/relocatable**: the linker interpret from the left side;
- **executable**: the loader interpret from the right side.


- 目标文件(object/relocatable file)需要链接器做进一步处理，所以一定有*Section Header Table*；
- 可执行文件(executable file)需要加载运 行，所以一定有*Program Header Table*；
- 而共享库既要加载运行，又要在加载时做动态链接， 所以既有*Section Header Table*又有*Program Header Table*。

read an ELF use `readelf`:
```
readelf -a max.o   
```


## ELF header

```
$ readelf -h ./program
ELF Header:
  Magic:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00
  Class:                             ELF64
  Data:                              2's complement, little endian
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI Version:                       0
  Type:                              EXEC (Executable file)
  Machine:                           Advanced Micro Devices X86-64
  Version:                           0x1
  Entry point address:               0x4022a0
  Start of program headers:          64 (bytes into file)
  Start of section headers:          93216 (bytes into file)
  Flags:                             0x0
  Size of this header:               64 (bytes)
  Size of program headers:           56 (bytes)
  Number of program headers:         12
  Size of section headers:           64 (bytes)
  Number of section headers:         37
  Section header string table index: 36
```

- _Start of program headers_ specifies the offset from the beginning of the file to the program header table;
- _Number of program headers_: number of entries of the program header table



## SECTION

> Roughly, a section is **a range of addresses**, with no gaps; all data “in” those addresses is treated the same for some particular purpose.
> 
> Sections hold the bulk of object file information for the linking view: _instructions_, _data_, _symbol table_, _relocation information_, and so on. 

![[Pasted image 20210817233645.png]]

> Section is the rigid unit that `ld` will arrange, i.e., `ld` will not break sections.
> An **object file** written by `as` has at least three sections, any of which may be empty. These are named `text`, `data` and `bss` sections.

- `.text` and `.data` 
	- These sections *hold your program*. `as` and `ld` treat them as separate but equal sections. Anything you can say of one section is true of another.
	- When the program is running, it is customary for the text section to be *unalterable*.  The text section is often shared among processes: it contains instructions, constants and the like. 
	- The data section of a running program is usually _alterable_: for example, C variables would be stored in the data section.
- `.bss` 
        	This section contains zeroed bytes when your program begins running. *It is used to hold uninitialized variables or common storage*. The length of each partial program's `.bss` section is important, but because it starts out containing zeroed bytes there is no need to store explicit zero bytes in the object file. The `.bss` section was invented to eliminate those explicit zeros from object files.
- `.data`  and `.data1` 
    hold _initialized data_ that contribute to the program’s memory image.
- `.rodata` and `.rodata1`
    These sections hold _read-only data_ that typically contribute to a non-writable segment in the process image.
- `.symtab`
    This section holds a symbol table
- `.plt` holds _procedure linkage table_
- `.got` holds [[Global Offset Table]]



### Section Header
```
$ readelf -SW program
There are 37 section headers, starting at offset 0x16c20:
...
```

Fields of each header:
1. _Name_: the name of the section;
2. _Type_: categorizes the section’s contents and semantics:
    1. _NULL_: inactive header, no section content
    2. _PROGBITS_: The section holds information defined by the program, whose format and meaning are determined solely by the program. e.g., `.text` section
    3. _SYMTAB_: provides symbols for link editing, though it may also be used for dynamic linking.
    4. _DYNSYM_: holds a minimal set of dynamic linking symbols, to save space.
    5. _STRTAB_: string table.
    6. _RELA_:  holds relocation entries with explicit addends
    7. _REL_: holds relocation entries without explicit addends
3. _Flag_: 
    1. _WRITE_: The section contains data that should be writable during process execution.
    2. _ALLOC_: The section occupies memory during process execution.
    3. _EXECINSTR_ The section contains executable machine instructions


### Symbol table

> An object file’s symbol table holds information needed to _locate_ and _relocate_ a program’s symbolic definitions and references. 
  

###### Symbol Binding Types:
1. `LOCAL`:  not visible outside the object file
2. `GLOBAL`: visible to all object files being combined
3. `WEAK`: also visible outside the object file, but weak symbols are ignored if the same symbol which is global exists (can be defined or undefined) somewhere else.

###### Symbol Type:
1. `OBJECT`: data object, such as a variable, an array, etc.
2. `FUNC`
3. `SECTION`: associated with a section
4. `FILE`: gives the name of the _source file_ associated with the object file.

When another object file references a function from a shared object, the link editor automatically creates a _procedure linkage table entry_ for the referenced symbol (with type `FUNC`).


###### Relocation

> _Relocation_ is the process of connecting symbolic references with symbolic definitions.
> A _relocation section_ references two other sections: _a symbol table and a section to modify_.

Relocatable files must have information that describes how to modify their section contents, thus allowing executable and shared object files to hold the right information for a process’s program image. _Relocation entries_ are these data.


## Segment

An object file _segment_ contains _one or more sections_.

- Executable and shared object files have a **base address**, which is the _lowest virtual address_ associated with the memory image of the program’s object file.



# Program Loading

As the system creates or augments a process image, it logically copies a file’s segment to a virtual memory segment. 


 
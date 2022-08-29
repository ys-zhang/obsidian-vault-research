# X86 

Registers in x86 architecture.
[x86 Registers](https://www.eecg.utoronto.ca/~amza/www.mindsec.com/files/x86regs.html)

## GPR (general purposed registers)

| size    | register                |                                 |
| ------- | ----------------------- | ------------------------------- |
| 64 bit  | RAX RBX RCX RDX         |                                 |
| 32 bits | EAX EBX ECX EDX         | E for extend                    |
| 16 bits | AX BX CX DX             | low part                        |
| 8  bits | AH AL BH BL CH CL DH DL | H for high part, L for low part |
 
 | register | name                 | convention                                             |
 |:--------:| -------------------- | ------------------------------------------------------ |
 |   EAX    | accumulator register | I/O, arith, interrupt calls ...                        |
 |   EBX    | base register        | base pointer for memory access, gets interrupt ret val |
 |   ECX    | counter register     | loop counter, shifts, gets interrupt val               |
 |   EDX    | data register        | same as EAX                                            |


## Segment registers

Segment registers hold the _segment address_ of various items.


| register | value             | description | 
| -------- | ----------------- | ----------- |
| CS       | code segment      |             |
| DS       | data segment      |             |
| ES FS GS | extra segment reg |             |
| SS       | stack segment     |             |


## Indexes and pointers

Indexes and pointer and the offset part of and address.

They some time used with a *segment register* to point to far address (in a 1Mb range). 

The register with an "E" prefix can only be used in _protected mode_.

 | register         | name                           | convention                                   |
 | ---------------- | ------------------------------ | -------------------------------------------- |
 | EDI with ES      | destination index register     | str/arr copy, for pointer addressing with ES |
 | ESI, EDI with DS | source index register          | str/arr copy                                 |
 | EBP with SS      | stack base pointer register    | holds base address of the stack              |
 | ESP with SS      | stack pointer register         | holds top address of the stack               |
 | EIP with CS      | index/program pointer register | (readonly) holds the offset of the nest instruction     |


## EFLAGS

The EFLAGS register hold the state of the processor.

The following is only for Intel.

| Bit (Position) | Label | Description                    |
| -------------- | ----- | ------------------------------ |
| 0              | CF    | carry flag                     |
| 2              | PF    | parity flag                    |
| 4              | AF    | aux carry flag                 |
| 6              | ZF    | zero flag                      |
| 7              | SF    | sign flag                      |
| 8              | TF    | trap flag                      |
| 9              | IF    | interrupt enable flag          |
| 10             | DF    | direction flag                 |
| 11             | OF    | overflow flag                  |
| 12-13          | IOPL  | I/O privilege level            |
| 14             | NT    | nested task flag               |
| 16             | RF    | resume flag                    |
| 17             | VM    | virtual 8086 mode flag         |
| 18             | AC    | alignment check flag           |
| 19             | VIF   | virtual interrupt flag         |
| 20             | VIP   | virtual interrupt pending flag |
| 21             | ID    | id flag                               |

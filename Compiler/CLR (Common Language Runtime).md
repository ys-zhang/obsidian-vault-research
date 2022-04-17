
# Concepts

## Managed Module
A **managed module** is a standard 32-bit Windows portable executable (PE32) file or a standard 64-bit Windows portable executable (PE32+) file that requires the CLR to execute.
![[Pasted image 20220226163244.png]]

A **managed module** includes:
1. PE32 or PE32+ header: part of the _Windows PE_ format;
2. CLR header: the version of the CLR required and other info used by CLR;
3. _Metadata_: 
    -  _types_ and _members_ defined in your source code 
    -  tables that describe the types and members referenced by your source code
4. IL code


> [!NOTE]
> Of all of the Microsoft compilers mentioned, _C++ is unique_ in that it is the only compiler that _allows the developer to write both managed and unmanaged code_ and have it emitted into a single module.


## Assembly

![[Pasted image 20220226164005.png]]

1. **Assembly** is a _logical grouping_ of one or more modules or resource files. 
2. An **assembly** is the _smallest unit_ of reuse, security, and versioning.
3. Each **assembly** you build can be _either an executable_ application _or a DLL_. 
4. The **assembly manifest** is simply another _set of metadata tables_. These tables describe the files that make up the assembly, the publicly exported types implemented by the files in the assembly, and the resource or data files that are associated with the assembly.


> [!NOTE]
> By default, compilers turning the emitted managed module into an assembly (with only one managed module an d no resources).
> 
> If you want to group a set of files into an assembly, you’ll have to be aware of more tools (such as the assembly linker, _AL.exe_) and their command-line options.



## CLR

1. After Windows has examined the EXE file’s header to determine whether to create a 32-bit or 64-bit process, Windows loads the x86, x64, or ARM version of `MSCorEE.dll` into the process’s address space. 
2. Then, the process’s primary thread calls a method defined inside MSCorEE.dll. This method initializes the CLR, loads the EXE assembly, and then calls its entry point method (`Main`). 

![[Pasted image 20220301145926.png]]
![[Pasted image 20220301150024.png]]

 ### Checking installed runtime: 
 
 ```shell
# dotnet and dotnet core
dotnet --list-runtimes 

# .NET Framework
# installed in 
#
# %SystemRoot%\Microsoft.NET\Framework
# %SystemRoot%\Microsoft.NET\Framework64
 ```


### CTS (Common Type System)

TLDR; _**CTS** specify how to define types in IL_.

Because types are at the root of the **CLR**, Microsoft created a formal specification—the Common Type System (CTS)—that _describes how types are defined and how they behave_.

CTS is part of CLI.

CTS specifies  types members:
- Field
- Method
- Property 
- Event.

Access modifier:
- Private
- Family/protected
- Assembly/internal
- Family and assembly 
- Public



### CLI (Common Language Interface)

A standard pack contains formats, metadata, IL, and access to the underlying platform (P/Invoke) for the purpose of standardization.

### CLS (Common Language Interface)

Common Language Specification (CLS) that details for compiler vendors the _minimum set of features their compilers must support_ if these compilers are to generate types compatible with other components written by other CLS-compliant languages on top of the CLR.

![[Pasted image 20220301155959.png]]



## App Domain
 The CLR does, in fact, offer the ability to _execute multiple managed applications in a single operating system process_. Each managed application executes in an `AppDomain`. 



 
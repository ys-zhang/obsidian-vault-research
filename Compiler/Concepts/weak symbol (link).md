A _weak symbol_ denotes a specially annotated symbol during _linking_ of [[ELF (Executable and Linkable Format) ]] object files. 

By default, without any annotation, a symbol in an object file is _strong_. 

During linking, a strong symbol can _override_ a weak symbol of the same name. 
In contrast, in the presence of two strong symbols by the same name, the linker resolves the symbol in favor of the first one found. 

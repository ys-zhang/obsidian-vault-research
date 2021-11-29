# Executable and Linkable Format

![[Pasted image 20210911115618.png]]

- **linkerable**: the linker interpret from the left side;
- **executable**: the loader interpret from the right side.


- 目标文件需要链接器做进一步处理，所以一定有*Section Header Table*；
- 可执行文件需要加载运 行，所以一定有*Program Header Table*；
- 而共享库既要加载运行，又要在加载时做动态链接， 所以既有*Section Header Table*又有*Program Header Table*。

read an ELF use `readelf`:
```
readelf -a max.o   
```


# SECTION

> Roughly, a section is **a range of addresses**, with no gaps; all data “in” those addresses is treated the same for some particular purpose.

![[Pasted image 20210817233645.png]]

> Section is the rigid unit that `ld` will arrange, i.e., `ld` will not break sections.
> An **object file** written by `as` has at least three sections, any of which may be empty. These are named **text**, **data** and **bss** sections.

- **text section**
- **data section**
	These sections **hold your program**. `as` and `ld` treat them as separate but equal sections. Anything you can say of one section is true of another. When the program is running, however, **it is customary for the text section to be unalterable. The text section is often shared among processes: it contains instructions, constants and the like**. 
	**The data section of a running program is usually alterable: for example, C variables would be stored in the data section.**
	
- **bss section**
	This section contains zeroed bytes when your program begins running. **It is used to hold uninitialized variables or common storage.** The length of each partial program's `bss` section is important, but because it starts out containing zeroed bytes there is no need to store explicit zero bytes in the object file. The `bss` section was invented to eliminate those explicit zeros from object files.
	

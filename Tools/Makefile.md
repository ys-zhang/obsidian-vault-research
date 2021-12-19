# Basic of gcc

![[Pasted image 20210911123430.png]]

![[Pasted image 20210912232039.png]]
##  Common CFLAGS
- `-L[dir]`: Add directory `dir` to the list of directories to be searched for `-l`.
- `-l[library]`
	- *Static libraries* are archives of object files, and have file names like `lib[library].a`.
	- *Shared libraries*, which typically have names like `lib[library].so`.
	-  The linker gives *preference to linking with the shared library* unless the `-static` option is used.
-  `-pthread`: Link with the POSIX threads library. 
-  `-I[dir]` (Include), Add the directory `dir` to the list of directories to be searched for *header files* during preprocessing.
- `-D[name]`, `-D[name=definition]`: predefine name as a macro, with definition `1`.


[Option Summary (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gcc/Option-Summary.html#Option-Summary)


# Makefile
Makefile由一组规 则（Rule）组成，每条规则的格式是:

```
target ... : prerequisites ... 
	command1 
	command2
	...
```

>目标和条件之间的关系是：欲更新目标，必须首先更新它的所有条件；所有 条件中只要有一个条件被更新了，目标也必须随之被更新。

>命令列表中的每条命令必须以一个Tab开头，注意不能是空格，Makefile的格式不像C语言的缩进那么随意，对于Makefile中的每个以Tab开头的命令，make会创建一个Shell进程去执行它


example: 
source files:`main.h`, `main.c`, `stack.h`, `stack.c`, `maze.h`, `maze.c`

```make

# default/first rule when execute `make`
main: main.o stack.o maze.o
	gcc main.o stack.o main.o -o main

main.o: main.c main.h stack.h maze.h
	gcc -c mian.c -o mian.o

stack.o: stack.c stack.h main.h
	gcc -c stack.c -o stack.o

maze.o: maze.c maze.h main.h
	gcc -c maze.c -o maze.o


# 如果make执行的命令前面加了@字符，则 不显示命令本身而只显示它的结果
# 如果命令前面加了-号，即使这条命令出错，make也会继续执 行后续命令。
clean: 
	@echo "cleanning project" 
	-rm main *.o 
	@echo "clean completed"

# .PHONY 是 make 内建的特殊目标，它声明一个伪目标
#     不管clean存在不存在都要更新
.PHONY: clean

```


execution of make
1. 首先从前到后读取所有规则，建立起一个完整的依赖关系图
2. 然后从缺省目标或者命令行指定的目标开始，根据依赖关系图选择适当的规则执行

rule name convention:

- `clean`: rm binary files
- `all`: compile usually default
- `install`: move compiled executable files
- `distclean`: 不仅删除编译生成的二进制文件，也删除其它生成的文件，例如配置文件和 格式转换后的文档，执行`make distclean`之后应该清除所有这些文件，只留下源文件。

# Variable

###### definition
- `foo = $(bar)` 延迟展开，可以把变量`bar`的值推迟到后面定义
- `:=` 立即展开
- `foo ?= $(bar)`：如果foo没有定义过，那么`?=`相当于`=`，定义foo的值是$(bar)，但不立即展开；如果先前已经定义了foo，则什么也不 做，不会给foo重新赋值。
- `+=`运算符可以给变量追加值

通常把`CFLAGS`定义成一些编译选项，例 如`-O`、`-g`等
把`CPPFLAGS`定义成一些预处理选项，例如`-D`、`-I`等

###### special variables
- `$@`，表示规则中的目标。 
- `$<`，表示规则中的第一个条件。 
- `$?`，表示规则中所有比目标新的条件，组成一个列表，以空格分割。
- `$^`，表示规则中的所有条件，组成一个列表，以空格分隔。


###### patterns

- `%.c`


# Function

syntax use function: 

    $(<func> <arg-list>)
    
 #### Common functions
 
 - if function
    - `$(if <condition>,<then-part> )`
    - `$(if <condition>,<then-part>,<else-part> )`
 - shell function execute a command
    -  `contents := $(shell cat foo)`
 - error function throw an error and cause `make` to stop
    - `$(error <text ...> )`
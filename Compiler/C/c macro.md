# 1. C编译器做语法解析之前的预处理步骤

1. 三连符替换成相应的单字符
2. 把用`\`字符续行的多行代码接成一行。
3. 把注释（不管是单行注释还是多行注释）都替换成一个空格
4. Lex 生成 preprocess token stream. C语言规定按照从前到后的顺序划分Token，每个Token都要尽可能长。
5. 在Token中识别出**预处理指示**，做相应的预处理动作，如果遇到`#include`预处理指示，则把相应的源文件包含进来，并对源文件做以上`1-4`步预处理。**如果遇到宏定义则做宏展开。**
6. 找出字符常量或字符串中的转义序列，用相应的字节来替换它，比如把\n替换成字节0x0a。
7. 把相邻的字符串连接起来。
8. 经过以上处理之后，把空白字符丢掉，把Token交给C编译器做语法解析，这时就不再是预 处理Token，而称为C Token了。


# 2. 宏定义

## 2.1 函数式宏定义 (function-like macro)

- 在定义时带一个参数，在调用时必须传一个参数给它，如果不传参数则表示传了一个空参数。

##### do {} while(0)

函数式宏定义经常写成这样的形式（取自内核代码`include/linux/pm.h`）：

```c
#define device_init_wakeup(dev,val) \ 
	do { \ 
		device_can_wakeup(dev) = !!(val); \ 
		device_set_wakeup_enable(dev,val); \ 
	} while(0)
```

原因：
1. macro expand to multiple stmt (error in single if-stmt)
```c
	if (n > 0) device_init_wakeup(d, v);
```
2. `while(0)` 为了用起来像function call 否则末尾无法加`;`


##### inline

inline关键字告诉编译器，这个函数的调用要尽可能快，可以当普通的函数调用实现，也可以 用宏展开的办法实现。

##### `#`、`##`运算符和可变参数

- 在函数式宏定义中，`#`运算符用于创建字符串，`#`运算符后面应该跟一个形参
```c
#define STR(s) # s 

/*
 用cpp命令预处理之后是"hello world"，
 自动用"号把实参括起来成为一个字符串，
 并且实参中的连续多个空白字符被替换成一个空格。
 */
STR(hello       world)
```
实参中的连续多个空白字符被替换成一个空格
- `##`运算符把前后两个**预处理Token**连接成一个**预处理Token**，和`#`运算符不 同，`##`**运算符不仅限于函数式宏定义，变量式宏定义也可以用**。
- gcc有一种扩展语法，如果`##`运算符用在`__VA_ARGS__`前面，除了起连接作用之外还有特殊的含 义，**comma separate list**.

定义一个宏展开成两个`#`号, 注意中间的两个空格是不 可少的，如果写成`####`，会被划分成`##`和`##`两个Token，而根据定义`##`运算符用于连接前后两个预处理Token，不能出现在宏定义的开头或末尾，所以会报错。
```c
#define HASH_HASH # ## #
```

###### 可变参数 `__VA_ARGS__`

函数式宏定义也可以带可变参数，同样是在参数列表中 用`...`表示可变参数。

```
#define showlist(...) printf(#__VA_ARGS__) 
#define report(test, ...) ((test)?printf(#test): printf(__VA_ARGS__))
```


# 2.2 宏展开的步骤

```c
#define sh(x) printf("n" #x "=%d, or %d\n",n##x,alt[x]) 
#define sub_z 26 
sh(sub_z)
```

1. `#x` 要替换成`sub_z`。 
2. `n##x`要替换成`nsub_z`。 
3. **除了带`#`和`##`运算符的参数之外，其它参数在替换之前要对实参本身做充分的展开**，所以 应该先把`sub_z`展开成`26`再替换到`alt[x]`中`x`的位置。
4.  现在展开成了`printf("n" "sub_z" "=%d, or %d\n",nsub_z,alt[26])`，
5.  **所有参数都替换完 了，这时编译器会再扫描一遍，再找出可以展开的宏定义来展开**，假设`nsub_z`或`alt`是变量式宏定义，这时会进一步展开.

# 2.3 条件预处理指示

```c
#ifndef HEADER_FILENAME 
#define HEADER_FILENAME 
/* body of header */ 
#endif

#if MACHINE == 68000
 int x;
#elif MACHINE == 8086
 long x;
#else /* all others */
 #error UNKNOWN TARGET MACHINE
#endif
```


# 2.4 Misc

`#pragma`预处理指示供编译器实现一些非标准的特性，C标准没有规定`#pragma`后面应该写什么以 及起什么作用，由编译器自己规定。

C标准规定了几个特殊的宏，在不同的地方使用可以自动展开成不同的值，常用的 有`__FILE__`和`__LINE__`，`__FILE__`展开为当前源文件的文件名，是一个字符串，`__LINE__`展开为 当前代码行的行号，是一个整数。

C标准规定`assert`应该实现为宏定义而不是一个真 正的函数，并且`assert(test)`这个表达式的值应该是`void`类型的。
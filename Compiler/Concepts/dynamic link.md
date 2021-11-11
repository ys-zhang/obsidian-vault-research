组成共享库的目标文件和一般的目标文件有所不同，在编译时要加`-fPIC`选项, `-f`后面跟一些编译选项，`PIC`是其中一种，表示生成位置无关代码（Position Independent Code）。

> 共享库各段的加载地址并没有定死，可以加载到任意位置，因为指令中没有使用绝 对地址，因此称为位置无关代码。


# audit

[rtld-audit(7) - Linux manual page (man7.org)](https://man7.org/linux/man-pages/man7/rtld-audit.7.html)

> The GNU dynamic linker (run-time linker) provides an auditing API that allows an application to be notified when various dynamic linking events occur.

> To use this interface, the programmer creates a shared library that implements a standard set of function names.


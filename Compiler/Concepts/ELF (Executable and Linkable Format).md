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


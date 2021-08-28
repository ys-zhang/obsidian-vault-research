一条 SQL 在 DB 接收到最终执行完毕返回，大致的过程如下：  
 1. 词法和语义解析；  
 2. 优化 SQL 语句，制定执行计划；  
 3. 执行并返回结果；

> 如上，一条 SQL 直接是走流程处理，一次编译，单次运行，此类普通语句被称作 **Immediate Statement**s （即时 SQL）

> 预编译语句的优势在于归纳为：**一次编译、多次运行**，省去了解析优化等过程；此外预编译语句能防止 SQL 注入。


# References

- [MySQL的SQL预处理(Prepared) - GeaoZhang - 博客园 (cnblogs.com)](https://www.cnblogs.com/geaozhang/p/9891338.html)
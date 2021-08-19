查看 调用 程序的用户是否有相关access权限(`F_OK`, `X_OK`, `R_OK`, `W_OK`); 
**有则返回`0`**
```
#include <unistd.h>

int access(const char *pathname, int mode);
```


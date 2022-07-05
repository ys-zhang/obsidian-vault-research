#cmd

`ls` list a directory
- `alias ll=ls -lh`
- `-a` show all ; `-A` show almost all
- `-l` use long format

| type-permission | num of links | owner | group | size(B) | lst-edit     | file name   |
| --------------- | ------------ | ----- | ----- | ------- | ------------ | ----------- |
| `-rwxr-xr-x`    | 1            | zys   | zys   | 110K    | Aug 15 13:12 | afl-analyze |


```
-rwxr-xr-x  1 zys zys 110K Aug 15 13:12 afl-analyze
-rw-r--r--  1 zys zys  24K Aug 15 12:47 afl-analyze.c
lrwxrwxrwx  1 zys zys    7 Aug 15 13:12 afl-clang -> afl-gcc
lrwxrwxrwx  1 zys zys    7 Aug 15 13:12 afl-clang++ -> afl-gcc
-rwxr-xr-x  1 zys zys  33K Aug 15 13:13 afl-clang-fast
lrwxrwxrwx  1 zys zys   14 Aug 15 13:13 afl-clang-fast++ -> afl-clang-fast
drwxr-xr-x  4 zys zys 4.0K Aug 15 12:47 docs
drwxr-xr-x 12 zys zys 4.0K Aug 15 12:47 experimental
drwxr-xr-x  2 zys zys 4.0K Aug 19 11:06 go-store
```

# permission
		file-type | owner's permision | grp permision | other's permision

- file-type: 

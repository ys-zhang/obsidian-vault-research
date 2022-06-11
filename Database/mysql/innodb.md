
# Architecture

![[Pasted image 20210730163354.png]]

# In Memory

- [[innodb buffer pool | Buffer bool]] is  where `InnoDB` caches `table` and `index` data as it is accessed. The algorithm is similar to [[JVM Garbage Collection| Generation GC]] [MySQL :: MySQL 8.0 Reference Manual :: 15.5.1 Buffer Pool](https://dev.mysql.com/doc/refman/8.0/en/innodb-buffer-pool.html)
- The [[innodb change buffer | change buffer]] is a special data structure that caches changes to secondary index pages when those pages are not in the [[innodb buffer pool | buffer pool]].
- [[adaptive hash index]]





# On Disk



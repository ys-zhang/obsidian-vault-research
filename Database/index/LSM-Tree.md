# Log-Structured Merge-Tree

Similar storage engines are used in *Cassandra* and *HBase*.

> Storage engines that are based on this principle of merging and compacting sorted files are often called LSM storage engines.

## SSTable (Sort Sorted Table)

An **SSTable** will consist of multiple sorted files called **_segment_**s.  With following properties:


- These segments are **immutable** once they are written to disk.
- key-value pairs within each segment are **sorted by the key**.
- key-value pairs within each segment are **unique**.
- in size of `MB`

![[Pasted image 20210731193544.png]]

## Memtable (in memory) (write data)


- underlying data structure is generally some form of a sorted tree like a [red-black tree](https://en.wikipedia.org/wiki/Red%E2%80%93black_tree).
- (**WRITE TO LSM-Tree**) writes get stored in this red-black tree until the tree reaches a predefined size. Once the red-black tree has enough entries, it is flushed to disk as a segment on disk in sorted order. This allows us to write the segment file as a single sequential write even though the inserts may occur in any order.
	![[Pasted image 20210731194132.png]]


## Sparse index (in memory) (read data)

![[Pasted image 20210731194716.png]]

- We can perform a binary search on our **sparse index** to find that `dollar` comes between `dog` and `downgrade`. Now we only need to scan from offset 17208 to 19504 in order to find the value (or determine it is missing).
- Use [[bloom filter]] to deal with case when `dollar` (the target) is missing.

##  Compaction

![[Pasted image 20210731195401.png]]

## Tomb stone (Deleting Data)

Deletes actually follow the exact same path as writing data. 
Whenever a delete request is received, a unique marker called a _tombstone_ is written for that key.

![[Pasted image 20210731195534.png]]


# Reference

Fay Chang, Jeffrey Dean, Sanjay Ghemawat, et al.: “*Bigtable: A Distributed Stor‐ age System for Structured Data*,” at 7th USENIX Symposium on Operating System Design and I
# Indexed Sequential Access Method


static index file, good for concurrency.

A static structure such as the ISAM index suffers from the problem that **long overflow chains** can develop as the file grows, leading to poor performance.


## Question

> To answer a range selection such as “Find all students with a GPA higher than 3.0,” we must identify the first such student by doing a binary search of the file and then scan the file from that point on. **If the file is large, the initial binary search can be quite expensive; can we improve upon this method?**

![[Pasted image 20210814183138.png]]

the index file consist of entries of  the form $<first_key_in_page, page_pointer>$.
![[Pasted image 20210814183349.png]]

However, a binary search of the index file could still be fairly expensive, and the index file is typically still large enough to make inserts and deletes expensive.

> The potential large size of the index file motivates the ISAM idea: Why not apply the previous step of building an auxiliary file on the index file and so on recursively until the final auxiliary file fits on one page?

![[Pasted image 20210814183607.png]]

- The data entries of the ISAM index are in the leaf pages of the tree and additional **overflow page**s that are chained to some leaf page.
- The ISAM structure is completely **static** (except for the overflow pages, of which it is hoped, there will be few) and facilitates such low-level optimizations.
- Each **tree node** is a disk page, and all the **data resides in the leaf pages**.
- If there are several inserts to the file subsequently, so that more entries are inserted into a leaf than will fit onto a single page, additional pages are needed because the index structure is static. These additional pages are allocated from an **overflow** area.
	> ![[Pasted image 20210814183929.png]]
- The fact that only leaf pages are modified also has an **important advantage with respect to concurrent access**.
	> In the ISAM structure, since we know that index-level pages are never modified, we can safely omit the locking step. Not locking index-level pages is an important advantage of ISAM over a dynamic structure like a B+ tree.


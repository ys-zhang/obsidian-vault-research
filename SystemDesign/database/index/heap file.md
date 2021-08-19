A **file of records** is a collection of records that may reside on several [[SystemDesign/database/index/data page]]s


# Heap File how a collection of pages can be organized as a file

```java

interface File {
	Record get(RID rid);
	void  insert(RID rid, Record rcd);
	boolean delete(RID rid);
	void scan(Func<Record, Boolean> filter);
}

```



The data in the pages of a **heap file** is not ordered in any way, and the only guarantee is that one can retrieve all records in the file by repeated requests for the next record.

We must keep track of the pages in each heap file in order to support **scans**, and we must keep track of pages that contain free space in order to implement **insertion** efficiently.

## Linked list of Pages

![[Pasted image 20210814181005.png]]

## directory of pages

An alternative to a linked list of pages is to maintain a directory of pages.
Each directory entry identifies a page (or a sequence of pages) in the heap file.
![[Pasted image 20210814182536.png]]
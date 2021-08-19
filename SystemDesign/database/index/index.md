An index is an auxiliary data structure that is intended to help us find rids of records that meet a selection condition.

Every index has an associated **search key**, which is a collection of one or more fields of the file of records for which we are building the index; any subset of the fields can be a search key. We sometimes refer to *the file of records* as the **indexed file**.

Organization techniques, or data structures, for index files are called **access method**s, and several are known including [[B+ tree]]s (Chapter 9) and hash-based structures (Chapter 10). B+ tree index files and hash-based index files are built using the page allocation and manipulation facilities provided by the disk space manager, just like heap files.


We have tree structured indexes like [[B+ tree]] and [[ISAM]] (Indexed Sequential Access Method).
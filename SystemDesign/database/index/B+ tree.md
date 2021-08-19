The B+ tree search structure, which is widely used, is a dynamic **balanced tree** in which the internal nodes direct the search and the leaf nodes contain the data entries.

B+ trees in which every node contains $m$ entries, where $d ≤ m ≤ 2d$. The value $d$ is a parameter of the B+ tree, called **the order of the tree**, and is a measure of the capacity of a tree node.

> If a file of records is updated frequently and sorted access is important, maintaining a B+ tree index with data records stored as data entries is almost always superior to maintaining a sorted file.
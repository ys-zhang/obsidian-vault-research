When a file is organized so that the ordering of data records is the same as or close to the ordering of data entries in some index, we say that the index is clustered.

it can be

1. A data entry $k*$ is an actual data record (with search key value $k$).
2. A data entry is a $\langle k, rid \rangle$ pair, where $rid$ is the record id of a data record with search key value $k$

![[Pasted image 20210814180035.png]]

> Indexes based on **hashing** do not store data entries in sorted order by search key, so a hash index is clustered only if it uses Alternative (1)
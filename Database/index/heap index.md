
> The **key** in an **index** is the thing that queries search for, but the **value** can be one of two things: it could be the **actual row (document, vertex)** in question, or it could be **a reference to the row stored elsewhere**.

> In the latter case, the place where rows are stored is known as a ** heap file**, and it stores data in **no particular order** (it may be append-only, or it may keep track of deleted rows in order to overwrite them with new data later). 

> The heap file approach is common because it **avoids duplicating data** when multiple secondary indexes are present: each index just references a location in the heap file, and the actual data is kept in one place.
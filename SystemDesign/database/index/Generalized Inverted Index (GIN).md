> GIN stores a list of keys with what's called a posting list of rows, each of which contain that key. A row can appear on the posting list of multiple keys too.


An **inverted index** is an index structure storing a set of (key, posting list) pairs, where 'posting list' is a set of documents in which the key occurs.

Generalized means that the index does not know which operation it accelerates.

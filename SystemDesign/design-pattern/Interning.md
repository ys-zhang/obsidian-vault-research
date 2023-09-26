#design-pattern

[wiki](https://en.wikipedia.org/wiki/Interning_(computer_science))

**interning** is re-using objects of equal value on-demand instead of creating new objects. It is a special case of [[flyweight pattern]]

>[!example]
>In many object-oriented languages such as Python, even primitive types such as integer numbers are objects. To avoid the overhead of constructing a large number of integer objects, these objects get reused through _interning_.

```python
"""
Enter _string_ in the table of “interned” strings and return the interned string – which is _string_ itself or a copy. 

Interning strings is useful to gain a little performance on dictionary lookup – if the keys in a dictionary are interned, and the lookup key is interned, the key comparisons (after hashing) can be done by a pointer compare instead of a string compare. 
Normally, the names used in Python programs are automatically interned, and the dictionaries used to hold module, class or instance attributes have interned keys.
"""
sys.intern(s: str)
```





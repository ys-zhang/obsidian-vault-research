#design-pattern 

享元模式 [wiki](https://en.wikipedia.org/wiki/Flyweight_pattern)


_Flyweight pattern_ or _hash consing_ refers to an object that minimises memory usage by sharing some data with other similar objects.

flyweight objects can:
 - store _intrinsic_ state that is invariant, context-independent and shareable (for example, the code of character 'A' in a given character set)
 - provide an interface for passing in _extrinsic_ state that is variant, context-dependent and can't be shared (for example, the position of character 'A' in a text document)




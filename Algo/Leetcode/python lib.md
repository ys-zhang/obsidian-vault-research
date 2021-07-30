# Bisect

```python
import bisect

bisect.bisect_left(a, x, lo=0, hi=len(a)): int
位置，left < x , right >=x

import itertools

# combinations('ABCD', 2) --> AB AC AD BC BD CD
# combinations(range(4), 3) --> 012 013 023 123
itertools.combinations(iterable, r)

chain('ABC', 'DEF') --> A B C D E F
itertools.chain(*iterables)
chain.from_iterable(iterable)

```

`functools.reduce(func, iterable[, initializer])`

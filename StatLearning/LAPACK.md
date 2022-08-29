[LAPACK Function Finding Advisor for Intel® oneAPI Math Kernel Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-function-finding-advisor.html#gs.810697)

# How routines are named

- [Routine Naming Scheme (netlib.org)](https://netlib.org/lapack/lug/node24.html): 
- [Driver Routines (netlib.org)](https://netlib.org/lapack/lug/node25.html)
- [Computational Routines (netlib.org)](https://netlib.org/lapack/lug/node37.html)
- [Matrix Storage Schemes (netlib.org)](https://netlib.org/lapack/lug/node121.html)

# Routines references:

[lapack man page (utah.edu)](https://www.math.utah.edu/software/lapack/)

# Some Critical Concepts

## Leading dimension of an array/matrix
[How Leading Dimension Is Used for Matrices - IBM Documentation](https://www.ibm.com/docs/en/essl/6.3?topic=matrices-how-leading-dimension-is-used)

Suppose matrices are store in _column-major_.

The **leading dimension** for a two-dimensional array is an _increment_ that is used to find the _starting point_ for the matrix elements in each successive _column_ of the array.

```
+---------------------------+
| 1.0 8.0  15.0 | 22.0 29.0 |    suppose we have a matrix M
| 2.0 9.0  16.0 | 23.0 30.0 |    stored in an array A[7][5],
| 3.0 10.0 17.0 | 24.0 31.0 |    at location A[0:5, 0:3]
| 4.0 11.0 18.0 | 25.0 32.0 |    then the leading dimention
| 5.0 12.0 19.0 | 26.0 33.0 |    is 7, since the next col of 
+---------------+           |    M is 7 elems awary in A.
| 6.0 13.0 20.0   27.0 34.0 | 
| 7.0 14.0 21.0   28.0 35.0 |
+---------------------------+
```
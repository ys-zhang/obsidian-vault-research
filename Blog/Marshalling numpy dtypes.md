#python 
#scientific-computing
#blog 

When considering binary data formats, we are talking about:  
  
* *type* of the data  
* *size* of each element  
* *byte order* of the format  
  
[Numpy dtype objects](https://numpy.org/doc/stable/reference/arrays.dtypes.html) describes these information  

| dtype             | info |
| ----------------- | ---- |
| `dtype.type`      | Text |
| `dtype.itemsize`  | Text |
| `dtype.byteorder` | Text |



# datetime64 & timedelta64

[Datetimes and Timedeltas — NumPy v1.22 Manual](https://numpy.org/doc/stable/reference/arrays.datetime.html)

## Internal storage

The unit for `datatime64` internal storage can be either 
- [date unit](https://numpy.org/doc/stable/reference/arrays.datetime.html#arrays-dtypes-dateunits):  years (‘Y’), months (‘M’), weeks (‘W’), and days (‘D’)
- or a [time unit](https://numpy.org/doc/stable/reference/arrays.datetime.html#arrays-dtypes-timeunits): hours (‘h’), minutes (‘m’), seconds (‘s’), milliseconds (‘ms’), and some additional SI-prefix seconds-based units.

> since version 1.11.0: NumPy does not store timezone information.

`timedelta64` data type was created to complement `datetime64`, for datatime64 substraction.

Datetimes are always stored based on POSIX time (though having a TAI mode which allows for accounting of leap-seconds is proposed), with an epoch of 1970-01-01T00:00Z.

This means the supported dates are always a symmetric interval around the epoch, called “time span”.


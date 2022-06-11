[An Introduction to the Python Buffer Protocol](https://jakevdp.github.io/blog/2014/05/05/introduction-to-the-python-buffer-protocol/)

[Buffer Protocol — Python 3.9.7 documentation](https://docs.python.org/3/c-api/buffer.html)

[PEP 3118 -- Revising the buffer protocol (python.org)](https://legacy.python.org/dev/peps/pep-3118/)

#UML 
```plantuml

split
	-[hidden]->
	:a:A;
split again
	-[hidden]->
	:b:B;
endsplit
:char* buf;
```

[NumPy Buffer Implementation: buffer.c](https://github.com/numpy/numpy/blob/main/numpy/core/src/multiarray/buffer.c)
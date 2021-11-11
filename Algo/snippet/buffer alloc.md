[go/source.go at master · golang/go (github.com)](https://github.com/golang/go/blob/master/src/cmd/compile/internal/syntax/source.go)

```go
type source struct {
	...
	buf       []byte // source buffer
	...
}

func (s *source) init(in io.Reader, 
					  errh func(line, col uint, msg string)) {
	...
	if s.buf == nil {
		s.buf = make([]byte, nextSize(0))
	}
	...
}

// nextSize returns the next bigger size for a buffer of a given size.
func nextSize(size int) int {
	const min = 4 << 10 // 4K: minimum buffer size
	const max = 1 << 20 // 1M: maximum buffer size which is still doubled
	if size < min {
		return min
	}
	if size <= max {
		return size << 1
	}
	return size + max
}
```


buffer size grow in double when `size <= max` 
[generate package - cmd/go/internal/generate - pkg.go.dev](https://pkg.go.dev/cmd/go/internal/generate)

Stringer is a tool to automate the creation of methods that satisfy the `fmt.Stringer` interface. Given the name of a (signed or unsigned) integer type T that has constants defined, stringer will create a new self-contained Go source file implementing:

	func (t T) Stringy() string

The file is created in the same package and directory as the package that defines T. It has helpful defaults designed for use with go generate.

# Options

- `-type` is a compulsory argument which defines which type the string belongs to.
- `-output` flag is used to explicitly specify the name of the file what will be generated. This will override the default name `<type>_string.go`
- `-trimprefix` trim the prefix before generating the final string output.
```go 
type HeroType int

//go:generate stringer -type=HeroType -trimprefix=HeroType
const (
    HeroTypeStrength HeroType = iota + 1
    HeroTypeAgility
    HeroTypeIntelligence
)
```
- `-linecomment` use the line comment string as the string representation
```go
type Artist int

//go:generate stringer -type=Artist -linecomment
const (
    ArtistLedZeppelin Artist = iota + 1 // led-zepplin
    ArtistPinkFloyd                     // pink-floyd
    ArtistPostMalone                    // post-malone
)
```
![[Pasted image 20210820100306.png]]

# General

## Unix socket

| network        | description                                                                                                                                                                                                       |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| SOCK_STREAM    | Provides sequenced, reliable, two-way, connection-based byte  streams.  An out-of-band data transmission mechanism may be supported.                                                                              |
| SOCK_DGRAM     | Supports datagrams (connectionless, unreliable messages of a fixed maximum length)                                                                                                                                |
| SOCK_SEQPACKET | Provides  a  sequenced,  reliable,  two-way connection-based data transmission path  for  datagrams  of  fixed maximum  length;  a  consumer  is  required  to read an entire packet with each input system call. |
   
  
## UDP

**it is may not a good idea to have a variant UDP packet size**

> Usually in a UDP protocol packet sizes are known in advance, and it's usually much smaller, in the order of 1.5k or less.
> What you can do is pre-allocate a maximum size static buffer for all reads, then once you know the size of the datagram you've read from the socket, allocate a byte array with the actual size and copy the data to it. I don't think you can do extra reads of the same datagram.
  
   

# golang 

types and functions are defined under package `net`

## Address

### MAC Layer

```go
type HardwareAddr []byte 
func ParseMAC(s string) (hw HardwareAddr, err error)
```

### IP Layer
```go
package net


// ================ Address types ==========================

type IP []byte // for ipv4 and ipv6 
type IPNet struct {    // IP network
    IP   IP     // network number
    Mask IPMask // network mask
}
type IPAddr struct {
    IP   IP
    Zone string // IPv6 scoped addressing zone / interface; see [[ipv6-scoped-zone]]
}


// ================ create address =========================

func IPv4(a, b, c, d byte) IP
func ParseIP(string) IP
func ParseCIDR(s string) (IP, *IPNet, error)

// lookup host (google.com), return IPV4 and IPV6 addresses
func LookupIP(host string) ([]IP, error)
```

### Transport Layer
```go
type TCPAddr struct {
    IP IP
    Port int
}
type UDPAddr struct {
    IP   IP
    Port int
    Zone string // IPv6 scoped addressing zone; added in Go 1.1
}

// network can be udp4 udp6 ...
func ResolveUDPAddr(network, address string) (*UDPAddr, error)
```

### Unix domain socket
```go
type UnixAddr struct {
    Name string
    Net  string // "unix", "unixgram" or "unixpacket"
}
func ResolveUnixAddr(network, address string) (*UnixAddr, error)
```

# Snippets
```go 
func handle(conn int) {

}

func main() {
	sockpath := "/tmp/afl-store.sock"
	sockfd, err := syscall.Socket(syscall.AF_UNIX, syscall.SOCK_SEQPACKET, 0)
	if err != nil {
		log.Fatal("Can't open unix socket")
	}

	if err := syscall.Access(sockpath, syscall.F_OK); err == nil {
		syscall.Unlink(sockpath)
	}

	if err := syscall.Bind(sockfd, &syscall.SockaddrUnix{Name: sockpath}); err != nil {
		log.Printf("Fail to bind to address\n")
		log.Fatalln(err)
	}
	if err := syscall.Listen(sockfd, 0); err != nil { // 0 is the length of backlog
		log.Fatalf("Fail to listen on the socket %v", err)
	}

	log.Printf("Listen on %s\n", sockpath)
	conn, _, err := syscall.Accept(sockfd)
	if err != nil {
		log.Println(err)
	}

	handle(conn)

	if err := syscall.Unlink(sockpath); err != nil {
		log.Fatalf("Fail to remove the socket %s\n", sockpath)
	}
	log.Printf("done")
}
```


```go

package main

import (
	"log"
	"syscall"
)

func fileExists(path string) bool {
	err := syscall.Access(path, syscall.F_OK)
	return err == nil
}

func createSock(bindTo string) int {
	if fileExists(bindTo) {
		err := syscall.Unlink(bindTo)
		if err != nil {
			log.Fatalln(err)
		}
	}
	sockfd, err := syscall.Socket(syscall.AF_UNIX, syscall.SOCK_SEQPACKET, 0)
	if err != nil {
		panic(err)
	}
	err = syscall.Bind(sockfd, &syscall.SockaddrUnix{Name: bindTo})
	if err != nil {
		panic(err)
	}
	return sockfd
}

func main() {
	sockpath := "/tmp/afl-store.sock"
	sockfd := createSock(sockpath)
	count := 0
	buffer := make([]byte, 70000)
	for count < 15000 {
		syscall.Recvfrom(sockfd, buffer, syscall.MSG)
	}
	handle(nfd)
}

func handle(nfd int) {
	syscall.Recvmsg()
}
```
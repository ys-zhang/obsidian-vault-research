![[IP-segment header.png]]

An IPv4 datagram consists of a **header** part and a body or **payload** part.

- The **Version** field keeps track of which version of the protocol the datagram belongs to.
- The **Total length** includes everything in the datagram—both header and data.
- The **Identification** field is needed to allow the destination host to determine which packet a newly arrived fragment belongs to. All the fragments of a packet  contain the same Identification value.  (datagram are fragmented)
-  **DF* stands for Don’t Fragment. It is used as part of the process to discover the path MTU, which is the largest packet that can travel along a path without being fragmented.
-  **MF** stands for More Fragments. All fragments except the last one have this  bit set. It is needed to know when all fragments of a datagram have arrived.
-  The **Fragment offset** tells where in the current packet this fragment belong
-  The **TTL** (Time to live) field is a counter used to limit packet lifetimes. (number of hops)

Since the header carries vital information such as addresses, it rates its own  checksum for protection, the Header checksum.


![[Pasted image 20210811001958.png]]



# IP Address

IP addresses are **hierarchical**, unlike Ethernet addresses. A network corresponds to *a contiguous block of IP address space*. This block is called a **prefix**.

![[Pasted image 20210806002656.png]]

-  Routers can forward packets based on only the *network portion* of the address.
-  With in the destination sub network. This makes the routing tables much smaller than they would otherwise be.
-  **The Broadcast Address is the last address of an IP network.**


> Addresses are **hierarchical** and can be allocated in **blocks**, e.g. 256 addresses in the block 128.18.3.0 – 128.18.3.255

### Private network address segments:
- 10.0.0.0/8 (16,777,216 hosts)
- 172.16.0.0/12 (1,048,576 hosts) 
- 192.168.0.0 /16 (65,536 hosts)

## Routing Table

![[Pasted image 20210811002146.png]]


##  NAT (Network Address Translation)  网关


- 端口映射

![[Pasted image 20210811002250.png]]


## Fragment

![[Pasted image 20210811001809.png]]


# ICP
![[Pasted image 20210811002405.png]]


- Ping uses ICMP

![[Pasted image 20210811002513.png]]

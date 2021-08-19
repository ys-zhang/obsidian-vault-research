# Problem

1. Providing a well-defined service interface to the network layer.
2. Dealing with transmission errors.
3. Regulating the flow of data so that slow receivers are not swamped by fast senders.

## Services
![[Pasted image 20210805162028.png]]

Base on underlying physical network we can choose:

1. Unacknowledged connectionless service ([[Ethernet]])
2. Acknowledged connectionless service ([[Wifi]])
3. Acknowledged connection-oriented service (have an order )

> It is perhaps worth emphasizing that providing acknowledgments in the data link layer is just an optimization, never a requirement.

## Data Framing

1. Byte count. 
2. Flag bytes with byte stuffing. 
3. Flag bits with bit stuffing. 
4. Physical layer coding violations.


#### Byte Staffing
![[Pasted image 20210805164213.png]]

#### Bit Staffing

![[Pasted image 20210805165132.png]]
Each frame begins and ends with a special bit pattern, `01111110` or `0x7E` in hexadecimal. This pattern is a **flag byte**.

Whenever the sender’s data link layer encounters five consecutive `1`s in the data, it automatically stuffs a `0` bit into the outgoing bit stream.

When the receiver sees five consecutive incoming `1` bits, followed by a `0` bit, it automatically destuffs (i.e., deletes) the `0` bit.

## [[Flow control]]

## Error Detection and Correction

In a **block code**, the $r$ *check bits* are computed solely as a function of the $m$ data bits with which they are associated, as though the $m$ bits were looked up in a large table to find their corresponding $r$ check bits.

In a **systematic code**, the $m$ data bits are sent directly, along with the check bits, rather than being encoded themselves before they are sent. (data bits are not changed)

In a **linear code**, the $r$ check bits are computed as a linear function of the $m$ data bits.


#### Error Detection

- [[Hamming codes]]
- [[Binary convolutional codes]]
- [[Reed-Solomon codes]]
- [[Low-Density Parity Check codes]]

#### Error Correction

- Parity
- checksum
- [[CRC]] (Cyclic Redundancy Check)



# Protocols

![[Pasted image 20210805211734.png]]

The protocol are often implemented in **NIC (hardware)** or **device driver (OS)**.


The physical layer process and some of the data link layer process run on dedicate hardware called a **NIC (Network Interface Card)**.    The rest of the link layer process and the network layer process run on the main CPU as part of the operating system, with the software for the link layer process often taking the form of a **device driver**.

## [[Flow control]]



## [[MAC sublayer]]

How to share the medium/channel 

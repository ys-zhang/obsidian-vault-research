

![[Pasted image 20210805143259.png]]

# Design Principle

1. A layer should be created where *a different abstraction is needed*.
2. Each layer should perform a *well-defined function*.
3. The function of each layer should be chosen with an eye toward defining internationally standardized protocols.
4. The layer boundaries should be chosen to *minimize the information flow across the interfaces.*
5. The number of layers should be large enough that *distinct functions need not be thrown together in the same layer* out of necessity and small enough that the architecture does not become unwieldy.


# [[Physical Layer]]

Transmitting raw bits over a communication channel. 
The design issues have to do with making sure that when one side sends a 1 bit it is received by the other side as a 1 bit, not as a 0 bit. 
Typical questions here are
- what electrical signals should be used to represent a 1 and a 0, 
- how many nanoseconds a bit lasts
- whether transmission may proceed simultaneously in both directions
- how the initial connection is established
- how it is torn down when both sides are finished
- how many pins the network connector has, and what each pin is used for. 
	
These design issues largely deal with mechanical, electrical, and timing interfaces, as well as the physical transmission medium, which lies below the physical layer.

# [[Data Link Layer]]

The main task of the data link layer is to transform a raw transmission facility into a line that *appears free of undetected transmission error*.
 
- (**bit semantics**) Define dataframe and provide **error detection/correction** of dataframe. Add semantics to bits, where is start of bits, where is end of bits.
- (**transition control**) how to keep a fast transmitter from drowning a slow receiver in data.
- **broadcast** and access of **shared** channel ([[MAC sublayer]])
	

# [[Network Layer]]

The network layer controls the operation of the subnet.

- how packets are **routed** from source to destination (since we have subnet)
- [[congestion control]], If too many packets are present in the subnet at the same time, they will get in one another’s way, forming bottlenecks.
- QoS (Quality of service), [[latency]], [[jitter]] 

# [[Transport Layer]]

The basic function of the transport layer is to accept data from above it
- split it up into smaller units if need be, pass these to the network layer, 
- and ensure that the pieces all arrive correctly at the other end.
- determine what type of service to upper layer
	- error-free P2P stream ([[TCP]])
	- parcel deliver without order / unique guarantee. ([[IP]])


# Other Layers

1. **Session Layer**.  The session layer allows users on different machines to establish sessions between them.
	1. dialog control
	2. token management
	3. synchronization
2. **Presentation Layer**. the presentation layer is concerned with the **syntax and semantics of the information** transmitted
3. **Application Layer**:
	1. [[HTTP]]
	2. [[HTTPS]]
	3. [[FTP]]
	4. [[DNS]]
	5. [[SSH]]
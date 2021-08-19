
# TCP Segment 
The TCP data stream is partitioned by TCP segments. The upper length limit of the TCP segment is bounded by MTU of data link layer.

The basic protocol used by TCP entities is the sliding window protocol with a dynamic window size.

![[TCP-segment-header.png]]

- The **Source port** and **Destination port** fields identify the local end points of the connection.
- Connection tuple (UID of the connection) `(dst_ip, dst_port, src_ip, src_port, protocol)`
- The **Sequence number** and **Acknowledgment number** fields are used for **sliding window**. *Note that the latter specifies the next in-order byte expected, not the last byte correctly received.*
- The **TCP header length** tells how many 32-bit words are contained in the TCP header
- **CWR** and **ECE** are used to signal congestion when **ECN (Explicit Congestion Notification)** is used.
	- **ECE** is set to signal an **ECN-Echo** to a TCP sender to tell it to slow down when the TCP receiver gets a congestion indication from the network. 
	- **CWR** is set to signal Congestion Window Reduced from the TCP sender to the TCP receiver so that it knows the sender has slowed down and can stop sending the **ECN-Echo**.
- **URG** is set to 1 if the _Urgent pointer_ is in use. The Urgent pointer is used to indicate a byte offset from the current sequence number at which urgent data are to be found. This facility is in lieu of interrupt messages.
- The **ACK** bit is set to 1 to indicate that the Acknowledgment number is valid.
- The **SYN** bit is used to establish connections.
- The **FIN** bit is used to release a connection.

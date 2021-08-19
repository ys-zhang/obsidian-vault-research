
# Sliding window

## Problem
 
The problem can be viewed as a consequence of the rule requiring a sender to wait for an acknowledgment before sending another frame.

Basically, the solution lies in allowing the sender to transmit up to $w$ frames before blocking, instead of just $1$.

This technique of keeping multiple frames in flight is an example of **pipelining**.

### Choose $w$

![[Pasted image 20210805220139.png]]

## Go back $N$

![[Pasted image 20210805220404.png]]

The other general strategy for handling errors when frames are pipelined is called **selective repeat**. When it is used, a bad frame that is received is discarded, but any good frames received after it are accepted and buffered. When the sender times out, only the oldest unacknowledged frame is retransmitted.

This type of acknowledgment is called a cumulative acknowledgment. This property is especially important when some of the previous acknowledgment-bearing frames were lost or garbled.

## [[MAC sublayer]]
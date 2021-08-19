Real physical connections are not P2P. multiple nodes can access the same channel.

In the literature, **broadcast channels** (not P2P channel) are sometimes referred to as **multi-access channels** or **random access channels**.

>    The protocols used to determine who goes next on a multi-access channel belong to a sublayer of the data link layer called the MAC (Medium Access Control) sublayer.


> Sit between [[Data Link Layer]] and [[Physical Layer]].

# Channel Allocation Problem

## Static Channel Allocation

-    **FDM** (Frequency Division Multiplexing)
-    **TDM** (Time Division Multiplexing)

## Dynamic Channel Allocation / Contention Algorithms

**Carrier Sense or No Carrier Sense**. With the carrier sense assumption, stations can tell if the channel is in use before trying to use it.

![[Pasted image 20210805224946.png]]
### ALOHA

##### Pure ALOHA

The basic idea of an ALOHA system is simple: **let users transmit whenever they have data to be sent**.

Senders need some way to find out **if transmission succeeded**. 

In the ALOHA system
- each station sent its frame to the central computer
- this computer rebroadcasts the frame to all of the stations
- A sending station can thus listen for the broadcast from the hub to see if its frame has gotten through.


#####  Slotted ALOHA

divide time into discrete intervals called slots, each interval corresponding to one frame.

### Carrier Sense Multiple Access Protocols
 
##### 1-persistent  CSMA (Carrier Sense Multiple Access)
 
When a station has data to send, 
1. Station first listens to the channel to see if anyone else is transmitting at that moment.
	- If the channel is idle, the stations sends its data.
	-  Otherwise, if the channel is busy, the station just waits until it becomes idle.
2. If a collision occurs, the station waits a random amount of time and starts all over again.


> The protocol is called 1-persistent because the station transmits with a probability of 1 when it finds the channel idle.


##### non-persistent  CSMA (Carrier Sense Multiple Access)

A station senses the channel when it wants to send a frame, 
- if no one else is  sending, the station begins doing so itself. 
- However, if the channel is already in use, the station does not continually sense it for the purpose of seizing it immediately upon detecting the end of the previous transmission. **Instead, it waits a random period of time and then repeats the algorithm.**

######    p-persistent CSMA

When a station becomes ready to send, it senses the channel. If it is idle, it transmits with a probability $p$. With a probability $q = 1 - p$, it defers until the next slot. If that slot is also idle, it either transmits or defers again, with probabilities $p$ and $q$.


######    CSMA/CD (CSMA with Collision Detection)

This protocol, known as **CSMA/CD** (CSMA with Collision Detection), is the **basis of the classic Ethernet LAN**, so it is worth devoting some time to looking at it in detail.

At the point marked $t_0$, a station has finished transmitting its frame. Any other station having a frame to send may now attempt to do so. If two or more stations decide to transmit simultaneously, there will be a collision. If a station detects a collision, it aborts its transmission, waits a random period of time, and then tries again.


# Implementation

- [[Ethernet]]


# MAC Address

>The MAC Address provides the unique identifier for a physical interface

>MAC Address is a 48-bit number encoded in the frame, written in hexadecimal notation



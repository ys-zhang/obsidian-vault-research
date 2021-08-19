
> How to connect Network of Networks?

![[Pasted image 20210811000736.png]]


The network layer is concerned with getting packets from the source all the way to the destination.

# Service

## Routing
1. The services should be **independent of the router technology**.
2. The transport layer should be shielded from the number, type, and topology of the routers present.
3. The **network addresses** made available to the transport layer should use a uniform numbering plan, even across LANs and WANs.


***Routing Table***:

| Prefix address | Subnet mask   | Interface |
| -------------- | ------------- | --------- |
| 203.32.8.0     | 255.255.255.0 | Eth0      | 



## Connect Networks
![[Pasted image 20210811001516.png]]
### Tunneling
> Tunneling is a special case used when the source and destination are on the same network, but there is a different network in between.
![[ipv6-tunneling.png]]

### Fragmentation

Fragmentation (division of packets into fragments) allows network gateways to meet size constraints

![[ip-fragment.png]]


# Routing Algorithms

> The routing algorithm is responsible for **deciding on which output line an incoming packet should be transmitted**

1. connectionless: packets are routed individually
	>routing table/algorithms
2. connection-oriented: packets uses the same route
	> Packets are routed through virtual circuits (created earlier) based on tag number (not full address but unique at a given link) in them


## Static Routing / Non-adaptive


### shorted path routing

> [[Sink Tree]]: the set of optimal routes from all sources to a given destination form a tree rooted at the destination

- [[Dijkstra's algorithm]]


### flooding

- **Flooding** :Every incoming packet is sent out on every outgoing line except the one on which it arrived

- **Selective flooding** (where routers send packets only on links which are in approximately the right direction) is an improved variation

## Dynamic Routing / Adaptive

### distance vector routing
1. Each node knows distance of links to its neighbors 
2. Each node advertises vector of lowest known distances to all neighbors
3. Each node uses received vectors to update its own 
4. Repeat periodically

### link state routing

1. Discover neighbors and learn network addresses 
2. Measure delay or cost to each neighbor 
3. Build link state packet 
4. Send this packet to all other routers 
5. Compute the shortest path to every other router, e.g. using Dijkstra’s algorithm

## Hierarchical routing


![[congestion-control.png]]
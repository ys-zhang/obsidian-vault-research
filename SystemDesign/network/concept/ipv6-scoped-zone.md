[Scope zones - IBM Documentation](https://www.ibm.com/docs/en/zos/2.3.0?topic=addressing-scope-zones)
[IPv6 Series Part 3: Address Scope and Scope Zone - Intense School](http://resources.intenseschool.com/ipv6-address-scope-and-scope-zone/)

Each IPv6 address has a specific **scope** in which it is defined.

> A **scope** is a topological area within which the IPv6 address can be used as a **unique** identifier for an interface or a set of interfaces.

The scope for an IPv6 address is encoded as part of the address itself.

> A **scope zone** is an instance of a given scope.

# unicast addr scope options

- link-local
- global scope

# multicast addr scope options

-   Interface-local
-   Link-local
-   Subnet-local
-   Admin-local
-   Site-local
-   Organization-local
-   Global scopes

# zone indices

How to specify the scope-zone of an ipv6 address.
![[Pasted image 20210820114039.png]]
With zone indices, an IPv6 address can be represented in the format:

	<address>%<zone_id>

The `zone_id` can be:
- number: `fe80::2%1` (HOST 1A)
- interface names: `fe80::2%eth0`

Different vendors will implement the zone indices differently. Windows operating systems, for example, use the interface index of the interface as the _zone_id_ for link-local addresses.

on windows: `netsh interface ipv6 show interface`
```
Idx     Met         MTU          State                Name
---  ----------  ----------  ------------  ---------------------------
  3          55        1500  connected     Wi-Fi
  1          75  4294967295  connected     Loopback Pseudo-Interface 1
  9          25        1500  disconnected  Local Area Connection* 1
 14          65        1500  disconnected  Bluetooth Network Connection
 17          25        1500  disconnected  Local Area Connection* 2
 42          15        1500  connected     vEthernet (WSL)
 11          25        1500  disconnected  Ethernet 2
 
Ethernet adapter vEthernet (WSL):

   Connection-specific DNS Suffix  . :
   Link-local IPv6 Address . . . . . : fe80::1a3:381e:9515:984f%42 
   IPv4 Address. . . . . . . . . . . : 192.168.224.1
   Subnet Mask . . . . . . . . . . . : 255.255.240.0
   Default Gateway . . . . . . . . . :
```
# example

Unique Local IPv6 unicast addresses are treated as having **global scope**. 
Also, the loopback address, ::1, is treated as having **link-local scope**. 
Finally, the unspecified address, ::, does not have any scope.
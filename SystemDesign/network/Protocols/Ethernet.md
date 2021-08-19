Standard for [[LAN]].

#  Classic Ethernet MAC Sublayer Protocol 

![[Pasted image 20210805230134.png]]

   Classic Ethernet uses the **1-persistent CSMA/CD** algorithm.

## MAC address

> An interesting feature of station source addresses is that they are **globally  unique**, assigned centrally by IEEE to ensure that *no two stations anywhere in the world have the same address.*

## Length of the dataframe

- upper limit: posed by hardware, RAM needs to be big enough to hold a dataframe.
- lower limit: for collision detection

![[Pasted image 20210811000325.png]]


# Switched Ethernet

The idea is partition the whole network in to multiple small **collision domains**.

In a [[hub]], all stations are in the same collision domain. They must use the CSMA/CD algorithm to schedule their transmissions. 

In a [[switch]], each port is its own independent collision domain.


![[Ethernet-Frame-Format.png]]
# Question/Aim

needs of coordination:

-   configuration
-   group membership
-   leader election
-   lock

```note
CHAIN OF IMPLEMENTATION
_needs of coordination ← coordination primitive ← coordination kernel_
```


# Properties/Guarantees

1.  wait-free
2.  FIFO-client ordering
3.  linearizablity of writes can be implemented by ZAB
4.  ZooKeeper processes read requests locally at each replica.
5.  if a client is watching for a change, the client will **see the notification event before it sees the new state of the system** after the change is made.

# Termology, Roles and API

-   _Server_: processes providing ZooKeeper service
    
-   _Client_: user of ZooKeeper
    
-   _Data tree_: data stored in ZooKeeper as a tree
    ![[Pasted image 20210910213755.png]]
-   _Znode_: a node of the data tree
	- _Regular znode_: explicitly create and delete;
	-   _Ephemeral znode_: explicitly create, auto or explicitly delete.

    When creating a new znode, a client can set a sequential flag, which will assign a monotone sequential number to the znode.
    If `n` is the new znode and `p` is the parent znode, then the sequence value of `n` is never smaller than the value in the name of any other sequential znode ever created under `p`.
   
   $$
  seq(\textrm{new\_node}) \ge seq(\textrm{node})\;\; \forall \textrm{node} \in descendent(par(\textrm{new\_node}))
  $$
origin: [Chubby: A lock service for distributed coordination | by Ameya | Coinmonks | Medium](https://medium.com/coinmonks/chubby-a-centralized-lock-service-for-distributed-applications-390571273052)

# Chubby: A lock service for distributed coordination


In this post, we will cover google’s [Chubby lock service](https://static.googleusercontent.com/media/research.google.com/en//archive/chubby-osdi06.pdf). This paper introduces a lock service, that can help with coordination in a distributed environment using locking semantics. Chubby is used extensively inside Google in various systems such as GFS, BigTable. The primary goal is to provide a reliable lock service. Chubby is NOT optimized for high performance, frequent locking scenarios. There are thousands of clients that use Chubby, but they use Chubby occasionally — for coarse-grained locking. Generally such coarse grained locks are held for hours or days and NOT seconds. One typical use of Chubby is for multiple applications is to elect a master — the first one getting the lock wins and becomes the master.


## Paxos or Lock Service?

If we expand on the example mentioned in the last section, this specific problem really converges to a problem of establishing consensus in a distributed system. So one could solve this by using paxos than to build a centralized lock service. For paxos to work, one could build a library and all applications would use that library to participate in consensus. Following points give a good idea of why lock service was chosen for the google ecosystem.

1.  **Developer friendliness**: Generally, it is far easier for developers to add locking semantics to their code than consensus based mechanics. Specially lot of applications don’t start out big and as they grow bigger, they add code to establish coordination. Most developers are also far more familiar with locking semantics.
2.  **Event notification with data**: In most cases, applications need to access small amount of data in such distributed environments -which means need for writing data and reading it in a consistent manner. This fits very well into locking mechanics.
3.  **Application progress**: In a paxos like setup, you need majority of applications to be up to make progress. In a centralized lock service mode, even if only one client is up, it can make progress as long as it has correct access to locks from Chubby.

## Design decisions

Following main design decisions come out from the topics mentioned in the last section.

1.  Coarse grained locking — Applications don’t need locks of shorter duration. For example, electing a master is not a frequent event.
2.  Small data storing(small file manipulations)capabilities in addition to a lock service
3.  Allow thousands of clients to observe changes. So lock service needs to scale to handle many clients, although the transaction rate may not be that high.
4.  Notification mechanism by which client knows when the change occurs in the file that is shared e.g. when a primary changes
5.  Support client side caching to deal with clients that may want to poll aggressively
6.  Strong caching guarantees to simplify developer usage
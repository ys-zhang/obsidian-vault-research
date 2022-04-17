#algorithm  #concurrency

# 银行家算法

[【操作系统】银行家算法 - 华为云 (huaweicloud.com)](https://www.huaweicloud.com/articles/c50896992b3d4ffc7cbd48acf5e257a1.html)

银行家算法的名字来源于该算法原本是为银行系统设计的，以确保银行在发放现金贷款时，不会发生不能满足所有客户需要的情况，在OS中可以用它来避免死锁。


为实现银行家算法，每一个新进程在进入系统时，必须申明在运行过程中可能需要每种资源类型的最大单元数目，其数目不应超过系统所拥有的资源总量。当进程请求一组资源时，系统必须首先确定是否有足够的资源分配给该进程。若有，再进一步计算在将这些资源分配给进程后，是否会使系统处于不安全状态。如果不会，才将资源分配给它，否则让进程等待。

For the Banker's algorithm to work, it needs to know three things:

-   How much of each resource each process could possibly request[MAX]
-   How much of each resource each process is currently holding[ALLOCATED]
-   How much of each resource the system currently has available[AVAILABLE]


Then we need the following data structures:

-   Available: A vector of length $m$ indicates the number of available resources of each type. If $Available[j] = k$, there are $k$ instances of resource type $R_j$ available.
-   Max: An $n \times m$ matrix defines the maximum demand of each process. If $Max[i,j] = k$, then $P_i$ may request at most $k$ instances of resource type $R_j$.
-   Allocation: An $n \times m$ matrix defines the number of resources of each type currently allocated to each process. If $Allocation[i,j] = k$, then process $P_i$ is currently allocated $k$ instances of resource type $R_j$.
-   Need: An $n \times m$ matrix indicates the remaining resource need of each process. If $Need[i,j] = k$, then $P_i$ may need $k$ more instances of resource type $R_j$ to complete the task.

Note: $Need[i,j] = Max[i,j] - Allocation[i,j]$. $n=m-a$.
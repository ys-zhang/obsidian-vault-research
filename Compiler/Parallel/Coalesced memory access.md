**Coalesced memory access** or **memory coalescing** refers to combining multiple memory accesses into a single transaction.
   
>The important thing to remember is that to ensure memory coalescing we want work-items from the same warp to access contiguous elements in memory so to minimize the number of required memory transactions.
#multi-threading 


# Concepts and Informal Model

## Happens-before

- Within a thread, happen before is introduced naturally same as language semantics, roughly speaking, same order as source code statements order.
- Happens before semantics is establish across threads through the [[#Release-Acquire Ordering|acquire and release semantics]]

> Let $A$ and $B$ represent operations performed by a multithreaded process. 
> If $A$ _**happens-before**_ $B$, then the memory effects of $A$ effectively become **visible** to the thread performing $B$ before $B$ is performed.

**Happens-before** does not mean _happening before_, happens-before only guarantees visibility, i.e., if $A$ happens-before $B$ but $A$ does not need to see $B$ then compiler reordering may happen.


## Data Dependency

Two machine instructions, executed in the same thread, are **data-dependent** whenever the first instruction outputs a value and the second instruction uses that value as input.

## Carries-a-dependency

it just says that one evaluation _carries-a-dependency_ to another if the value of the first is _used as an operand_ of the second. It’s kind of like the language-level version of a machine-level [[#Data Dependency]]. (There is actually a strict set of conditions for what constitutes _carrying-a-dependency_ in C++11 and what does not, but I won’t go into the details here.)


# Ordering Models

##  Relaxed Ordering

No synchronization, i.e. imposes no restrictions on ordering and visibility.
It only guarantees atomicity.


## Release-Acquire Ordering

Threads:
1. Thread 1 evaluation $A$:  `x.store(RELEASE)`
2. Thread 2 evaluation $B$:  `x.load(ACQUIRE)`
Then all _memory writes (non-atomic and relaxed atomic)_ that **happened-before** the atomic store $A$ from the point of view of thread 1, become **visible side-effects** in thread 2.

**In other words, once the atomic load is completed, thread B is guaranteed to see everything thread A wrote to memory.**

>[!WARNING]
>The synchronization is established only between the threads _releasing_ and _acquiring_ the same atomic variable.
> 
>_Other threads_ can see **different order** of memory accesses than either or both of the synchronized threads.




## Release-Consume Ordering

Threads:
1. Thread $1$ evaluation $A$:  `x.store(RELEASE)`
2. Thread $2$ evaluation $B$:  `x.load(CONSUME)`

Then all _memory writes (non-atomic and relaxed atomic)_ that **happened-before** the atomic store $A$ from the point of view of thread $1$, become __visible side-effects__ within those operations in thread $2$ into which the load operation [[#Carries-a-dependency]].

**In other words, once the atomic load is completed, those operators and functions in thread $2$ that use the value obtained from the load are guaranteed to see what thread $1$ wrote to memory.**

>[!CARRIES DEPENDENCY]
>Within the same thread, evaluation $A$ that is _sequenced-before_ evaluation $B$ may also **carry a dependency into** $B$ (that is, $B$ depends on $A$), if any of the following is true
>1. The value of $A$ is used as an operand of $B$, **except**
>    - if $B$ is a call to [std::kill_dependency](https://en.cppreference.com/w/cpp/atomic/kill_dependency "cpp/atomic/kill dependency")
>    - if $A$ is the left operand of the built-in `&&`, `||`, `?:`, or `,` operators.
>2. $A$ writes to a _scalar object_ $M$, $B$ reads from $M$
>3. $A$ carries dependency into another evaluation $X$, and $X$ carries dependency into $B$

>[!WARNING]
>The synchronization is established only between the threads _releasing_ and _acquiring_ the same atomic variable.
> 
>_Other threads_ can see **different order** of memory accesses than either or both of the synchronized threads.


## Sequentially-consistent ordering

This is equivalent to [[#Release-Acquire Ordering]] + Total Order, i.e., all threads see the same ordering.



# References

- [`std::memory_order` - cppreference.com](https://en.cppreference.com/w/cpp/atomic/memory_order)
- [Order of evaluation - cppreference.com](https://en.cppreference.com/w/cpp/language/eval_order)
- [`std::atomic_thread_fence` - cppreference.com](https://en.cppreference.com/w/cpp/atomic/atomic_thread_fence)
- [c++ - How do "acquire" and "consume" memory orders differ, and when is "consume" preferable? - Stack Overflow](https://stackoverflow.com/questions/19609964/how-do-acquire-and-consume-memory-orders-differ-and-when-is-consume-prefe)
- [The Purpose of memory_order_consume in C++11 (preshing.com)](https://preshing.com/20140709/the-purpose-of-memory_order_consume-in-cpp11/) this is the most helpful blog
- [The Happens-Before Relation (preshing.com)](https://preshing.com/20130702/the-happens-before-relation/)
- [C++ Data-Dependency Ordering: Atomics and Memory Model (open-std.org)](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2664.htm)
- [Acquire and Release Semantics (preshing.com)](https://preshing.com/20120913/acquire-and-release-semantics/)

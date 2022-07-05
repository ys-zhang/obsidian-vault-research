#concurrency 
#Rust 

Rust pretty blatantly just inherits the memory model for atomics from C++20. The C++ memory model is fundamentally about trying to bridge the gap between the _semantics_ we want, the optimizations _compilers_ want, and the inconsistent chaos our _hardware_ wants.


Reordering:
1. compiler reordering
2. hardware reordering
    - strongly-ordered: x86/64
    - weakly-ordered: arm  


# Happen before

The C++ memory model attempts to bridge the gap by allowing us to talk about the _causality(happen before relationship)_ of our program. The way we communicate  happen before relationships are through _data accesses_ and _atomic accesses_.

###### Data Access 
Data accesses are fundamentally unsynchronized, both compilers and hardware are free to aggressively optimize them.


###### Atomic Access
Each atomic access can be marked with an _ordering_ that specifies what kind of relationship it establishes with other accesses. In practice, this boils down to telling the compiler and hardware certain things they _can't_ do.


## Ordering
```rust
#[non_exhaustive]
pub enum Ordering {
    Relaxed,
    Release,
    Acquire,
    AcqRel,
    SeqCst,  // Sequentially Consistent
}
```


###### Sequentially Consistent

same as [sequentially-consistent in c++](https://en.cppreference.com/w/cpp/atomic/memory_order#Sequentially-consistent_ordering)

Intuitively, a sequentially consistent operation cannot be reordered: all accesses on one thread that happen before and after a `SeqCst` access stay before and after it.


###### Acquire-Release
Intuitively, an _acquire access_ ensures that _every access after it stays after it_. However operations that occur before an acquire are free to be reordered to occur after it. 

Similarly, a _release access_ ensures that _every access before it stays before it_. However operations that occur after a release are free to be reordered to occur before it.

```rust
let lock = Arc::new(AtomicBool::new(false)); // value answers "am I locked?"

// ... distribute lock to threads somehow ...

// Try to acquire the lock by setting it to true
while lock.compare_and_swap(false, true, Ordering::Acquire) { }
// broke out of the loop, so we successfully acquired the lock!

// ... scary data accesses ...

// ok we're done, release the lock
lock.store(false, Ordering::Release);
```
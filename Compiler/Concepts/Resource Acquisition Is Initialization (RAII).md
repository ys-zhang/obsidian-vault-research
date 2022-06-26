

# The Problem

In C++ when acquiring some resource, the resources must be handled properly to keep code safe, e.g. avoid, memory leak, dead lock, threads leak etc.

Simply put, **RAII is when you acquire resources in a constructor and release them in the corresponding destructor**.

Acquiring resources in a constructor and releasing them in the corresponding destructor _binds resource lifetime to object lifetime_, and makes it so that we no longer have to remember to explicitly release resources whenever they’re acquired.

**A rule of thumb in C++ is that you should never write** `**new**` **or** `**delete**`. Instead, you should use an _RAII object_ which manages the memory, i.e. you should couple memory allocation and deallocation to object lifetime.


# Examples

```c++
void lockMutexBad(bool shouldThrow) {
  std::cout << "Locking mutex manually..." << std::endl;
  globalMutex.lock();
  std::cout << "Mutex is locked!" << std::endl;

  // Could also imagine this as an early return. If you use a plain old
  // mutex, you have to remember to manually unlock before every return...
  // and it's quite easy to forget.
  if (shouldThrow) {
    std::cout << "Throwing exception" << std::endl;
    throw 1;
  }

  globalMutex.unlock();
  std::cout << "Mutex has been unlocked manually" << std::endl;
}

void lockMutexBadExample() {
  try {
    lockMutexBad(true);
  } catch (...) {
    std::cout << "Caught exception" << std::endl;
  }
  lockMutexBad(false);
}

void lockMutexGood(bool shouldThrow) {
  std::cout << "Locking mutex with scoped_lock..." << std::endl;
  // `std::scoped_lock` is an RAII class, 
  // and releases the lock in its destructor.
  std::scoped_lock lock(globalMutex);
  std::cout << "Mutex is locked!" << std::endl;

  if (shouldThrow) {
    std::cout << "Throwing exception" << std::endl;
    throw 1;
  }
}

void lockMutexGoodExample() {
  try {
    lockMutexGood(true);
  } catch (...) {
    std::cout << "Caught exception" << std::endl;
  }
  lockMutexGood(false);
}


```



# References

1. [What is RAII?. RAII stands for “resource acquisition… | by Matt Lim | The Startup | Medium](https://medium.com/swlh/what-is-raii-e016d00269f9)
2. [Resource acquisition is initialization - Wikipedia](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization)
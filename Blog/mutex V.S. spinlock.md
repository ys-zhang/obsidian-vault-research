#concurrency 
#iOS
#blog 

[synchronization - When should one use a spinlock instead of mutex? - Stack Overflow](https://stackoverflow.com/questions/5869825/when-should-one-use-a-spinlock-instead-of-mutex)


# The Theory

- (**mutex**) In theory, when a thread tries to lock a mutex and it does not succeed, because the mutex is already locked, it will go to sleep, immediately allowing another thread to run. It will continue to sleep until being woken up, which will be the case once the mutex is being unlocked by whatever thread was holding the lock before. 

- (**spinlock**)When a thread tries to lock a spinlock and it does not succeed, it will continuously re-try locking it, until it finally succeeds; thus it will not allow another thread to take its place (however, the operating system will forcefully switch to another thread, once the CPU runtime quantum of the current thread has been exceeded, of course).

# The Problem

The problem with mutexes is that putting threads to sleep and waking them up again are both rather expensive operations, they'll need quite a lot of CPU instructions and thus also take some time. <mark class="hltr-yellow">If now the mutex was only locked for a very short amount of time, the time spent in putting a thread to sleep and waking it up again might exceed the time the thread has actually slept by far and it might even exceed the time the thread would have wasted by constantly polling on a spinlock.</mark> On the other hand, polling on a spinlock will constantly waste CPU time and if the lock is held for a longer amount of time, this will waste a lot more CPU time and it would have been much better if the thread was sleeping instead.


# The Solution

<mark class="hltr-yellow">Using spinlocks on a single-core/single-CPU system makes usually no sense</mark> , since as long as the spinlock polling is blocking the only available CPU core, no other thread can run and since no other thread can run, the lock won't be unlocked either. IOW, a spinlock wastes only CPU time on those systems for no real benefit. If the thread was put to sleep instead, another thread could have ran at once, possibly unlocking the lock and then allowing the first thread to continue processing, once it woke up again.

On a multi-core/multi-CPU systems, with plenty of locks that are held for a very short amount of time only, the time wasted for constantly putting threads to sleep and waking them up again might decrease runtime performance noticeably. When using spinlocks instead, threads get the chance to take advantage of their full runtime quantum (always only blocking for a very short time period, but then immediately continue their work), leading to much higher processing throughput.


# The Practice

Since very often programmers cannot know in advance if mutexes or spinlocks will be better (e.g. because the number of CPU cores of the target architecture is unknown), nor can operating systems know if a certain piece of code has been optimized for single-core or multi-core environments, most systems don't strictly distinguish between mutexes and spinlocks. <mark class="hltr-orange">In fact, most modern operating systems have hybrid mutexes and hybrid spinlocks.</mark> What does that actually mean?

<mark class="hltr-orange">A hybrid mutex behaves like a spinlock at first on a multi-core system.</mark> If a thread cannot lock the mutex, it won't be put to sleep immediately, since the mutex might get unlocked pretty soon, so instead the mutex will first behave exactly like a spinlock. Only if the lock has still not been obtained after a certain amount of time (or retries or any other measuring factor), the thread is really put to sleep. If the same code runs on a system with only a single core, the mutex will not spinlock, though, as, see above, that would not be beneficial.

A hybrid spinlock behaves like a normal spinlock at first, but to avoid wasting too much CPU time, it may have a back-off strategy. It will usually not put the thread to sleep (since you don't want that to happen when using a spinlock), but <mark class="hltr-orange">it may decide to stop the thread (either immediately or after a certain amount of time; this is called "yielding") and allow another thread to run, thus increasing chances that the spinlock is unlocked</mark> (you still have the costs of a thread switch but not the costs of putting a thread to sleep and waking it up again).


# Summary

If in doubt, use mutexes, they are usually the better choice and most modern systems will allow them to spinlock for a very short amount of time, if this seems beneficial. Using spinlocks can sometimes improve performance, but only under certain conditions and the fact that you are in doubt rather tells me, that you are not working on any project currently where a spinlock might be beneficial. You might consider using your own "lock object", that can either use a spinlock or a mutex internally (e.g. this behaviour could be configurable when creating such an object), initially use mutexes everywhere and if you think that using a spinlock somewhere might really help, give it a try and compare the results (e.g. using a profiler), but be sure to test both cases, a single-core and a multi-core system before you jump to conclusions (and possibly different operating systems, if your code will be cross-platform).


## Update: A Warning for iOS

Actually not iOS specific but iOS is the platform where most developers may face that problem: <mark class="hltr-green">If your system has a thread scheduler, that does not guarantee that any thread, no matter how low its priority may be, will eventually get a chance to run, then spinlocks can lead to permanent deadlocks.</mark> The iOS scheduler distinguishes different classes of threads and threads on a lower class will only run if no thread in a higher class wants to run as well. There is no back-off strategy for this, so if you permanently have high class threads available, low class threads will never get any CPU time and thus never any chance to perform any work.

The problem appears as follow: Your code obtains a spinlock in a low prio class thread and while it is in the middle of that lock, the time quantum has exceeded and the thread stops running. The only way how this spinlock can be released again is if that low prio class thread gets CPU time again but this is not guaranteed to happen. You may have a couple of high prio class threads that constantly want to run and the task scheduler will always prioritize those. One of them may run across the spinlock and try to obtain it, which isn't possible of course, and the system will make it yield. The problem is: A thread that yielded is immediately available for running again! Having a higher prio than the thread holding the lock, the thread holding the lock has no chance to get CPU runtime. Either some other thread will get runtime or the thread that just yielded.

Why does this problem not occur with mutexes? When the high prio thread cannot obtain the mutex, it won't yield, it may spin a bit but will eventually be sent to sleep. A sleeping thread is not available for running until it is woken up by an event, e.g. an event like the mutex being unlocked it has been waiting for. Apple is aware of that problem and has deprecated `OSSpinLock` as a result. The new lock is called `os_unfair_lock`. This lock avoids the situation mentioned above as it is aware of the different thread priority classes. If you are sure that using spinlocks is a good idea in your iOS project, use that one. Stay away from `OSSpinLock`! And under no circumstances implement your own spinlocks in iOS! If in doubt, use a mutex. macOS is not affected by this issue as it has a different thread scheduler that won't allow any thread (even low prio threads) to "run dry" on CPU time, still the same situation can arise there and will then lead to very poor performance, thus `OSSpinLock` is deprecated on macOS as well.
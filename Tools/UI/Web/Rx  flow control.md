#flow-control 

# audit

When it sees a source value, it **ignores that** plus the next ones for `duration` milliseconds, and then it emits the most recent value from the source.

`audit` is similar to `throttle`, but emits the **last** value from the silenced time window, instead of the first value.

![[Pasted image 20220917123746.png]]

# throttle

Lets a value pass, then ignores source values for the next `duration` milliseconds.
![[Pasted image 20220917124052.png]]

# delay


# debounce 

It's like `delay`, but passes only the most recent notification from each burst of emissions.
![[Pasted image 20220917124201.png]]
delays notifications emitted by the source Observable, but drops previous pending delayed emissions if a new notification arrives on the source Observable. This operator keeps track of the most recent notification from the source Observable, and emits that only when `dueTime` has passed without any other notification appearing on the source Observable.



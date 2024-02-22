#reactive-programming 

# Reactive Programming

- [Introduction to Rx (introtorx.com)](http://introtorx.com/)
- [FSharp.Control](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control.html#category-1_1)
- [Observable (FSharp.Core)](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control-observablemodule.html)
- [Functional Reactive Programming | F# for fun and profit (fsharpforfunandprofit.com)](https://fsharpforfunandprofit.com/posts/concurrency-reactive/)
- [ReactiveUI - A new way of thinking by Kent Boogaart - YouTube](https://www.youtube.com/watch?v=A_qmik32Kos)


>[!NOTE]
> The _event driven paradigm_ allows for code to be invoked without the need for breaking encapsulation or applying expensive polling techniques. 
> 
> This is commonly implemented with the _Observer pattern_, events exposed directly in the language (e.g. C#) or other forms of _callback via delegate registration_. 
> 
> The **Reactive Extensions** extend the callback metaphor with LINQ to enable querying sequences of events and managing concurrency.



# Foundation Types `IObservable` & `IObserver`

```csharp
//Defines a provider for push-based notification.
public interface IObservable<out T>
{
  // Notice the parameter of type IObserver
  // which is `T -> unit` in the f# version
  IDisposable Subscribe(IObserver<T> observer);
}

//Provides a mechanism for receiving push-based notifications.
public interface IObserver<in T>
{
  //Provides the observer with new data.
  void OnNext(T value);
  //Notifies the observer that the provider has experienced an error condition.
  void OnError(Exception error);
  //Notifies the observer that the provider has finished sending push-based notifications.
  void OnCompleted();
}

```

```fsharp
var Observer.subscribe<'a> :
    ('a -> unit) -> IObservable<'a> -> IDisposable
```


> [!NOTE]
> Rx has an implicit contract that must be followed:
> 
> An implementation of `IObserver<T>` may have zero or more calls to `OnNext(T)` followed optionally by a call to either `OnError(Exception)` or `OnCompleted()`.



# Observable Semantics

1. _LRU(infinite)/Replay_ : caching all published values and then replaying them for any late subscriptions.   (rx.net: `ReplaySubject<T>` )
2. _LRU(1)_: only caching latest publish for late subscriptions. (rx.net:  `BehaviorSubject<T>`)
3. _async_ : only cache the last value and only publish when complete (rx.net: `AsyncSubject<T>`)

## Different Type of `IObservable`

| C#                  | F#  | Semantics               | Sequence |
| ------------------- | --- | ----------------------- | -------- |
| `Observable.Return` |     | replay                  | [v]      |
| `Observable.Empty`  |     | complete at one         | []       |
| `Observable.Never`  |     | never complete          | []       |
| `Observable.Throw`  |     | throw exception at once | []       |

![[Pasted image 20220211172048.png]]


# Frameworks



## Combine

#swift 

```swift
protocol Publisher { 
  associatedtype Output
  associatedtype Failure
  func subscribe<S: Subscriber>(s : S) -> AnyCancellable
    where S.Output  == Output,
          S.Failure == Failure
}
```

>[!NOTE]
>Generally, you do not need to implement a [`Publisher`](https://developer.apple.com/documentation/combine/publisher), instead,
> 
> - use predefined publishers such as `@Published`, `URLSession.dataTaskPublisher`, `NotificationCenter.publisher`
> - use the builtin pushers defined under the namespace of [`Publishers`](https://developer.apple.com/documentation/combine/publishers);
> - use one of [`Subject`](https://developer.apple.com/documentation/combine/subject) publishers ([`PassthroughSubject`](https://developer.apple.com/documentation/combine/passthroughsubject) & [`CurrentValueSubject`](https://developer.apple.com/documentation/combine/currentvaluesubject)), which is a publisher allowing you to "inject" values into the stream represented by it.



>[!Warning] Publisher is value type
> A publisher is usually a value type. This may lead to wield result.
> 
> For instance, subscribe twice to one single `URLSession.Publisher` will make 2 network requests.
> ```swift
> let pub1 = URLSession.share.dataTaskPublisher(...)
> let sub11 = pub1.sink(...)  // make the 1st http request 
> let sub12 = pub1.sink(...)  // make the 2nd http request 
>
> let pub2 = URLSession.share.dataTaskPublisher(...).share()
> let sub21 = pub2.sink()  // make the 1st http request
> let sub22 = pub2.sink()  // make no requests
> ```
> 
> `share()` available for all publishers and it allows you to make sure that multiple subscribers for a single publisher will ***all receive the same values without triggering more work than needed***.



### Life cycle

In Combine, publishers only perform work, and they only publish values if 
1. there is a subscriber listening and 
2. if the subscriber is willing to receive values.
in other words, _Combine_ is a ***pulling*** framework


When a publisher is subscribed by a subscriber user `Publisher.subscribe()`, the function returns a [`AnyCancellable`](https://developer.apple.com/documentation/combine/anycancellable) object,  calling its `.cancel()` method or the object is freed will tigger the end of the subscription, free resources used by the subscription allocated in the publisher, such as timers, network access or disk IO.
 

>[!Warning] Potential Memory Leak
> usually you need to store the `AnyCancellable` instance to keep the subscription alive. 
> 
> However when the publisher completes, although the allocated resources of the subscription is freed, _the `AnyCancellable` instance itself will not be freed automatically_.
> 
> Use the `.sink()` transformer to free the `AnyCancellable` instance


> [!Note] use `.flatmap` transformer
> `.flatmap()` is analog to _bind of the publisher monad_.
> `.setFailureType()` as the natural morphism between the publisher functor. 



### Multiple Subscribes Behaviour


> Now, what happens when, halfway through `A` streaming its data, a new observer `D` subscribes to `B`, totally unaware that `B` is already in the middle of its output?

```                          
                             +---+
                     +------>| C |
+---+     +---+      |       +---+
| A |---->| B |------|
+---+.    +---+      |       +---+
                     +------>| D |
                             +---+
```

1. _multicast_: `D` receives the second half of the values that `C` receives;
2. _caching_: the first half is buffered and `D` immediately receives the first half of the message upon joining and new values like multicast;
3. _latest value(caching 1)_: `D` receives the last emitted value immediately and new values like multicast;
4. _custom caching_: `D` receives only as much as needed (e.g. since the last keyframe or resume point) and new values like multicast;
5. _resubscribe_: `D` should trigger all upstream nodes to restart their work, go all the way back to the network and re-request all data, performing all calculations, again

| behaviour | transformer |
| ---- | ---- |
| multicast | `Publishers.Multicast` |
| caching | no default support |
| latest value | `CurrentValueSubject` |
| custom caching | no default support |
| resubscribe | default behaviour |

#### The event-queue mental model

What are the problems that affect event queues?
1. events which overwhelm their consumers
2. ~~consumers which block other processing while waiting on events~~
3. events on multiple threads triggering memory races
4. events on multiple threads triggering logical races

>[!TLDR]
>_Combine_ adopts the cold publisher/observable model, publishers only feed downstream subscribers only when those subscribers have told it a positive demand, else the value is dropped (when the publisher is a subject which is a hot publisher).

>[!WARNING] Pitfall of async and Demand
>
>The subscription occurs immediately but the `request` (demand) for unlimited values happens only after we let the main scheduler run. Until that point, values we might expect to be sent are completely lost.

Combine uses a concept called _“demand”_ (an implementation of what is elsewhere called “back-pressure”) to determine how many values a downstream subscriber will accept. When an upstream event source generates more values than the subscriber will accept, Combine throws the values away.

_subscribers_ (or more accurately subscription) are "run" on a `Scheduler`.

```swift
protocol Scheduler { 
  func schedule(_ action: @escaping () -> Void)
}
```

the `schedule` method put the action in some waiting list and guarantee it will be performed in the future.

The `Scheduler` is very much like a `Executor` in Rust's stackless green-thread async approach.

Note that as cold publishers, Combine only process values when subscribers reports positive demand when upstream publishers calls `Subscriber.receive(value) -> Demand` method of downstream subscribers. the _receive_ function is executed on the subscriber's _scheduler_, which may run on a different thread with the thread that `Subject.send`ing values, thus some values maybe dropped.

>[!TAKEAWAY] 
> use `buffer` whenever concurrency is needed, i.e., when ever `Publisher.receive`  or `Publisher.subscribe` is used.
>
> ```swift
> let ioPerformingPublisher = ...
> let uiUpdatingSubscriber = ...
> ioPerformingPublisher
>   .subscribe(on: backgroundQueue)
>   .receive(on: RunLoop.main)
>   .subscribe(uiUpdatingSubscriber)
>```
>
> the `subscribe(on:options:)` operator causes `ioPerformingPublisher` to receive requests on `backgroundQueue`, while the `receive(on:options:)` causes `uiUpdatingSubscriber` to receive elements and completion on `RunLoop.main`.

#### Subject

> [!Note] Subject Publisher
> A _subject_ in Combine is a special kind of _publisher_ that allows the developer, that’s you, to _inject values into its stream_.
> 
> In other words, a subject allows you to ***determine which values are published, and more importantly, when***.
> 
> Subjects in Combine have a `send(_:)` method that allows you to send values down the publisher’s stream of values. It’s also possible to use `send(completion:)` to complete a stream of values if needed.

There are 2 subject publishers:
1. [`PassthroughSubject`](https://developer.apple.com/documentation/combine/passthroughsubject) 
2. [`CurrentValueSubject`](https://developer.apple.com/documentation/combine/currentvaluesubject)
the difference is the _buffer size_.

### Merging Publishers

1. `Publishers.Zip`
2. `Publishers.Merge`
3. `Publishers.CombineLatest`


#### Zip

If only one of the two (or three, or four) zipped publishers emits a new value, we won’t immediately receive this value. This means that if one of the two publishers completes before the other publisher is completed, you won’t get any new values from the uncompleted publisher because the already completed publisher doesn’t emit new values anymore.

Note that `Publishers.Zip` will zip the values from its publishers in order. This means that if one of the publishers emits three values before the second publisher emits a value, the first value from the first publisher is matched up with the first value from the second publisher.


![[Pasted image 20240131181431.png]]

#### Merge

![[Pasted image 20240131184644.png]]


#### Combine Latest



### Time & flow control related transformers

#### Debouncing

> The principle of waiting a little while before processing user input is called debouncing.
> 
> Whenever a publisher emits several values in a short timeframe, these values are never delivered to the subscriber. The subscriber will only receive values that were _not followed up_ by a new value within the specified threshold (300 milliseconds can be a good choice).

![[Pasted image 20240128154345.png]]

#### Remove duplicates

> Use [`removeDuplicates()`](doc://com.apple.documentation/documentation/combine/publisher/removeduplicates()?language=swift) to remove repeating elements from an upstream publisher. This operator has a two-element memory: the operator uses the current and previously published elements as the basis for its comparison.

![[Pasted image 20240128161129.png]]

#### Throttle

_Throttle_ publishes either the most-recent or first element published by the upstream publisher in the specified time interval.

It selectively republish elements from an upstream publisher during an interval you specify. Other elements received from the upstream in the throttling interval aren’t republished.

While _debounce_ resets its cooldown period for every received value, the _throttle_ operator will emit values at a given interval.
![[Pasted image 20240128162527.png]]
![[Pasted image 20240128162541.png]]

### References

- 22 short tests of Combine
  1. [re-implementing the core protocols of Combine](https://www.cocoawithlove.com/blog/twenty-two-short-tests-of-combine-part-1.html)
  2. [shared computation, shared reference lifetimes and sharing subscribers](https://www.cocoawithlove.com/blog/twenty-two-short-tests-of-combine-part-2.html)
  3. [asynchrony, threading and performance](https://www.cocoawithlove.com/blog/twenty-two-short-tests-of-combine-part-3.html)
- Wals, D. (2020). _Practical Combine: An introduction to Combine with real examples_.


## Other frameworks

1. Reactive UI
  - [ReactiveUI - A new way of thinking by Kent Boogaart - YouTube](https://www.youtube.com/watch?v=A_qmik32Kos)
# Reactive Programming

- [Introduction to Rx (introtorx.com)](http://introtorx.com/)
- [FSharp.Control](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control.html#category-1_1)
- [Observable (FSharp.Core)](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control-observablemodule.html)
- [Functional Reactive Programming | F# for fun and profit (fsharpforfunandprofit.com)](https://fsharpforfunandprofit.com/posts/concurrency-reactive/)
- [ReactiveUI - A new way of thinking by Kent Boogaart - YouTube](https://www.youtube.com/watch?v=A_qmik32Kos)

```ad-note
The _event driven paradigm_ allows for code to be invoked without the need for breaking encapsulation or applying expensive polling techniques. 
This is commonly implemented with the _Observer pattern_, events exposed directly in the language (e.g. C#) or other forms of _callback via delegate registration_. 
The **Reactive Extensions** extend the callback metaphor with LINQ to enable querying sequences of events and managing concurrency.
```


# Foundation Types `IObservable` & `IObserver`

```c#
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

```ad-note
Rx has an implicit contract that must be followed:

An implementation of _IObserver<T>_ may have zero or more calls to _OnNext(T)_ followed optionally by a call to either _OnError(Exception)_ or _OnCompleted()_.
```

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

## Reactive UI
- [ReactiveUI - A new way of thinking by Kent Boogaart - YouTube](https://www.youtube.com/watch?v=A_qmik32Kos)

## [[Akka]]
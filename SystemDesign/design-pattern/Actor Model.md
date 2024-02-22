#concurrency #swift #actor-model 

> [!Definition] Actor
> An _actor_ is a computational entity that, in response to a message it receives, can _concurrently_:
> 
> - send a finite number of messages to other Actors;
> - create a finite number of new Actors; 
> - designate the behaviour to be used for the next message it receives.
>
> An _actor_ can _only_ communicate with _actors_ to which it is _connected_. It can directly obtain information only from other Actors to which it is directly connected.

The Actor model is characterised by 
- (message pathing) inherent concurrency of computation within and among Actors,
- (adding nodes) dynamic creation of Actors
- (adding edges) inclusion of Actor addresses in messages
- (closure) and interaction only through direct asynchronous message passing with no restriction on message arrival order.

>[!Definition] Locality
> _Locality_ means that in processing a message: an _actor_ can send messages only to _addresses_ for which it has information by the following means;
>
> 1. that it receives in the message
> 2. that it already had before it received the message
> 3. that it creates while processing the message.


# Actor in swift

>[!Property] synchronised field access
actors allow only one task to access their _mutable state_ at a time, which makes it safe for code in multiple tasks to interact with the same instance of an actor.

We use method and property access as ways to sending messages between actors.

Accessing actor $a$'a property from a different actor $b$ can be modelled as: 
1. sending msg $b\to a$ with info
    - $b$'s actor address
    - please send me a message containing property $p$'s value.
2. sending msg $a\to b$  with info of property $p$'s value.
This justifies why we need `await` when accessing an actor's property when outside of the actor.

>[!Quote] [The Swift Actor Proposal](https://github.com/apple/swift-evolution/blob/main/proposals/0306-actors.md)
>The second form of permissible cross-actor reference is one that is performed with an asynchronous function invocation. 
>
>Such asynchronous function invocations are turned into "messages" requesting that the actor execute the corresponding task when it can safely do so. 
>These messages are stored in the actor's "mailbox", and the caller initiating the asynchronous function invocation may be suspended until the actor is able to process the corresponding message in its mailbox.
> 
>***An actor processes the messages in its mailbox one-at-a-time, so that a given actor will never have two concurrently-executing tasks running actor-isolated code.***
>
>This ensures that there are no data races on actor-isolated mutable state, because there is no concurrency in any code that can access actor-isolated state.


> Actor-isolated functions are [reentrant](https://en.wikipedia.org/wiki/Reentrancy_(computing)). When an actor-isolated function suspends, reentrancy allows other work to execute on the actor before the original actor-isolated function resumes, which we refer to as _interleaving_.

- The function `Task.init` by creates a task _on current actor_.


# Actor in dotnet

![[Akka]]
#actor-model #concurrency 


# Actors

![[Pasted image 20220211195225.png]]

- Actors are always organised into a _tree_.
- The first of light is the `System` actor which serves the creator of _top level_ actors.
  ```f# 
  val IActorRef.ActorOf : unit -> IActorRef 
  ```
- _guardians_ are actors _supervising_ every actor living as a child of them, i.e. under their path.
    - `/` the so-called _root guardian_.
    - `/system` the _system guardian_.
    - `/user` the _user guardian_. **This is the parent actor for all user created actors**.
- `System.ActorOf()` creates an actor _directly under_ `/user`, which is referred as an _top level_ actor.

# Lifecycle
- No sub-component can outlive the parent component.
- A component handles the failure of its sub-components.
- `PreStart()` is invoked after the actor has started but before it processes its first message.
- `PostStop()` is invoked just before the actor stops. No messages are processed after this point.
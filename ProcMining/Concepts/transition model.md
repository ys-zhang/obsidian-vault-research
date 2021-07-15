## Transition Model

![[transition-model.png]]

A transition system is a triplet $TS = (S,A,T )$:

- $S$ is the set of **states**
- $A \subset \mathcal{A}$ is the set of **activities/actions**
- $T \subset S \times A \times S$ is the set of **transitions**
- $S_{start} \subset S$ is the set of **initial/start states**
- $S_{end} \subset S$ is the set of **final/accept states**

类似于 [[Finite state autometa (FSA)]]

问题：太多states。

### Deadlock and livelock

- A path **deadlocks** if it reaches a non-final state without any outgoing transitions.
- The transition system may **livelocked**, i.e., some transitions are still enabled but it is impossible to reach one of the final states.
[The Use of Likely Invariants as Feedback for Fuzzers | USENIX](https://www.usenix.org/conference/usenixsecurity21/presentation/fioraldi)

# Problem and Observation

A bug is triggered only when 
1. Program execution reaches a given _instruction_ and, 
2. The _state_ of the application satisfies certain _conditions_.

CGF fuzzers do not have any incentives to explore more states for an already observed set of control-flow facts.

However, it is hard to take program state into consideration:
1. Rewarding fuzzers for exploring new states (_state coverage_) is a poor strategy, which often decreases the bug detection rate.
2. Have the problem of _state explosion_.


# Old Approaches

> Reduce the program state into something more manageable to explore during testing.

1. _Approximate the program state by using more sensitive feedbacks_, like code coverage enriched with call stack information, or even with values loaded and stored from memory.

2. _Use prior knowledge about the target_, lets the developers define their domain-specific objectives and then adds waypoints that reward a fuzzer when a generated testcase makes progress towards those objectives


# New Approach

> The key idea is to augment edge coverage—the most widely-adopted and successful code coverage metric used by fuzzers—with information about **local divergences** from ‘usual’ variable values. 


## Steps
1. Run CGF for 24h to _collect samples_ from saved seeds.
2. Use samples to _learn local likely invariants_. 
3. Prune invariants.
   1. Plain fact with _no information_;
   2. Invariants combine _unrelated variables_;
   3. Whenever different invariants have _overlapping condition_.
4. Divide state space using these invariants. 
5. For each edge, generate a different value for the novelty search for each unique combination of violated invariants. 


## Properties

1. Resulting invariants often capture local properties of the test suite more than static properties of the program.
2. State space partition avoids state explosion. 
3. Each basic block has only a few invariants. Since only a few variables are involved.
4. Not all invariants are equally useful.

# Implementation

## Question

1. How we _define the state_ we want to capture in our invariants;
2. How we _perform the instrumentation_ of the program under test to collect the information required by our technique.


## Compile-time transformation phases

![[Pasted image 20211111145158.png]]
 1. _Learning phase_, where we emit logging instrumentation for program state variables to feed the invariant miner;
 2. _Instrumentation phase_, where we augment the code of the program under test to evaluate the likely invariants in a form directly suitable for coverage-guided fuzzers.


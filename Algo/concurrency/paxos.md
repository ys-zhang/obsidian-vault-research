[Neat Algorithms - Paxos - Will You Harry Me](http://harry.me/blog/2014/12/27/neat-algorithms-paxos/)

# Read (as a learner)
To read a value from the basic Paxos system, a client asks all the processes in the system what they have stored for the current value, and then takes the value that the majority of the processes in the system hold.
If there is no majority or if not enough processes respond, the read fails.

# Write (as a proposer)

The *basic Paxos algorithm* governs the flow for the *writing of just one new value*, which is then repeated to make the thing actually useful.

**Paxos makes a guarantee that clients can send their write requests to any member of the Paxos cluster. **

The property is important and neat: it means that there is no single point of failure, which means our Paxos governed system can continue to be online (and useful) when _any_ node goes down for whatever unfortunate yet unavoidable reason.

**A proposal is sent to the whole system by way of a `prepare` message from the process the client contacted to all the other processes it knows of.**

This `prepare` message holds inside it the value being proposed, as well as what’s called a  **sequence number** inside it.

**This sequence number is key: it allows processes to differentiate between newer and older proposals.**

If two processes are trying to get a value set, Paxos says that value proposed last should take precedence, so this lets processes figure out which one is last, and thus who is trying to set the most recent value.


1. **check one**: is the *sequence number* on an incoming `prepare` message the highest I have ever seen? If it is, then cool, I can prepare to accept this incoming value, and disregard any others I have heard of before.





# Safety Requirements

1.  Only one value can be chose
2.  The chosen value must be proposed
3.  A process never learn a value has been chosen unless it actually has.


# Simple Paxos

> The algorithm ensures that at most one command can be chosen.

## Role of Agents

-   `Proposer`: make a proposal
-   `Acceptor`: vote for proposals
-   `Learner`: learn that some value has been chosen and the chosen result


## Choosing a value

-   A proposal has a UID
-   `Accepter` can accept multiple proposals
-   An `acceptor` must accept the **first** proposal that it receives.
-   For any $v$ and $n$, if a proposal with value $v$ and number $n$ is issued, then there is a set $S$ consisting of **a majority of** `acceptor` such that either
	1. no `acceptor` in $S$ has accepted any proposal numbered less than $n$, 
	2. or `v` is the value of the highest-numbered proposal among all proposals numbered less than `n` accepted by the `acceptors` in $S$.

## Learning a chosen value

Answer the question of _what's the chosen value_.

the `acceptors` could respond with their acceptances to _some set of distinguished learners_, each of which can then inform all the learners when a value has been chosen.


## Liveness

It’s easy to construct a scenario in which two proposers each keep issuing a sequence of proposals with increasing numbers, none of which are ever chosen.

A distinguished proposer must be selected as the only one to try issuing proposals.

-   If the distinguished proposer can communicate successfully with a majority of `acceptors`
-   and if it uses a proposal with number greater than any already used, then it will succeed in issuing a proposal that is accepted.

> The famous result of *Fischer, Lynch, and Patterson* implies that a reliable algorithm for electing a proposer must use either randomness or real time—for example, by using timeouts.


## Algorithm

1. _Prepare request (Phase 1) (proposer):_
    
    A proposer chooses a new proposal number $n$ and sends a request to each member of some set of `acceptors`, asking it to respond with:
    
    1.  A promise never again to accept a proposal numbered less than $n$, and
    2.  The proposal with the highest number less than $n$ that it has accepted, if any.

If the proposer receives the requested responses from a majority of the `acceptors`, then it can issue a proposal with number $n$ and value $v$, where $v$ is the value of the highest-numbered proposal among the responses, or is any value selected by the proposer if the responders reported no proposals.

2.  _Accept request (Phase 2) (proposer):_

if the proposer received response from a majority of `acceptors`, then it send an accept request to a majority of `acceptors` with the proposal (number $n$, value $v$).

3.  _Accept proposal (acceptor):_

accept unless it has already responded to a prepare request having a number greater than $n$.

**Theorem:** If the algorithm has a chosen value $v$, then it will not choose another different value. **The consensus algorithm ensures that at most one command can be chosen.** proof:

suppose $p_1$, $p_2$ are proposal with number $n_1$, $n_2$ with $n_1 < n_2$ and value $v_1, v_2$. $v_1$ has been chosen, we need to prove, $v_2 = v_1$.

assume that $v_1$has already been chosen before prepare request of $p_2$; thus the value of $p_2$must be $v_1$. else if the prepare request of $p_2$ before the chosen of $v_1$and $v_2 \neq v_1$; we have $p_1$will not be chosen for a majority since the majority promised not to accept any proposal with number less than $n_2$


## Multi-Paxos

To guarantee that all servers execute the same sequence of state machine commands, we implement _a sequence of separate instances of the Paxos consensus algorithm_, the value chosen by the $i$th instance being the $i$th state machine command in the sequence.

1.  A single server is elected to be the leader, which acts as the distinguished proposer; also be the only learner.
2.  The new leader, being a learner in all instances of the consensus algorithm, should know most of the commands that have already been chosen. Suppose it knows commands 1–134, 138, and 139—that is, the values chosen in instances 1–134, 138, and 139 of the consensus algorithm. It then executes phase 1 of instances 135–137 and of all instances greater than 139.
3.  The leader can propose command 142 before it learns that its proposed command 141 has been chosen.
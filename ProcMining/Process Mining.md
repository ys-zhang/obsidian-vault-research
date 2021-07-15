#process-mining

# Process Mining

content from the book *Process Mining in Action.*

> *“Process Mining is a process management technique that allows for the analysis of business processes based on event logs.”*
—Wiki

Question → [Process Mining] → find out what happened → Why it happens?

# Question

- How to merge processes (mining) and decision making?

# Concepts

### Types of Process mining

![[proc-mining-big-pic.png]]

- **Discovery**. A discovery technique takes an ***event log*** and produces a ***model*** without using any prior information.
- **Conformance**. Here, an existing process ***model*** is compared with an ***event log*** of the same process. Conformance checking can be used to check *if reality, as recorded in the log, conforms to the model and vice versa*.
- **Enhancement**. Here, the idea is to extend or improve an existing process model using information about the actual process recorded in some event log.

### Model V.S. Reality

One of the key elements of process mining is the emphasis on establishing a strong relation between a process model and “reality” captured in the form of an event log.

![[proc-mining-exec-type.png]]

- **Play-out** refers to the classical use of process models. Given a Petri net, it is possible to generate ***behavior***.
- **Play-in** is the opposite of **Play-out**, i.e., example behavior is taken as input and
the goal is to construct a model. Play-in is often referred to as inference.
- **Replay** uses an event log and a process model as input. The event log is “replayed” on top of the process model.
    - **Conformance checking**. Discrepancies between the log and the model can be detected and quantified by replaying the log.
    - Extending the model with frequencies and temporal information.
    - Constructing predictive models.

### Event logs

An ***event log*** is a collection of events which have taken place in order to perform a business process.

Each ***event*** refers to a specific ***activity*** that took place at a ***certain moment in time*** and can be assigned to a unique ***case***.

```go
type EventLog struct {
	CaseID     int    // a run/case of some process
 	ActivityID int    // an activity, i.e. event itself 
	Timestamp         // when the event happens
  // other activity related attributes
}
```

# Process Modeling

## Equivalence of models

> When are two processes the same from *a behavioral point* of view

- ***Trace equivalence*** considers two transition systems to be equivalent if their *execution sequences are the same*.
- ***branching bisimilarity*** also take the *moment of choice* into account


## [[transition model]]
## [[petri net]]

## Workflow net

**Workflow net** is a labeled [[petri net]], with an *input and an output place and a short-circuit transition.*

After the short-circuit transition is included, the net become strongly connected, i.e. there exist a directed path between every pair of nodes.

### Soundness (whether represents a correct process)

Let $N = (P,T,F,A,l)$ be a **WF-net** with input place $i$ and output place $o$.

- (**safeness**) $(N,[i])$ is safe, i.e., places cannot hold multiple tokens at the same time.
- (**proper completion**) for any marking $M \in [N,[i]\rangle, o \in M$ implies $M = [o]$;
- (**option to complete**) for any marking $M \in [N,[i]\rangle, [o] \in [N,M\rangle$;
- (**absence of dead parts**) $(N,[i])$ contains no dead transitions (i.e., for any $t \in T$ , there is a firing sequence enabling $t$).

## YAWL (Yet Another Workflow Language)

types of transitions/tasks:

- **AND-join/AND-split**: (all or nothing)  it needs to consume one token via each of the incoming arcs and produces a token along each of the outgoing arcs.
- **XOR-split** selects precisely one of its *outgoing arcs*. The selection is based on evaluating data conditions. Only one token is produced and sent along the selected arc.
- An **XOR-join** is enabled once for every incoming token and does not need to synchronize.
- An **OR-split** selects one or more of its outgoing arcs.
- The **OR-join** requires at least one input token, but also **synchronizes** tokens that are “on their way” to the OR-join.

YAWL also supports **cancellation regions**. A task may have a cancellation region consisting of conditions, tasks, and arcs. Once the task completes all tokens are removed from this region.

![[proc-mining-yawl.png]]

## Business Process Modeling Notation (BPMN)

Difference with **YAWL** is that the *routing logic* is not associated with tasks but with separate *gateways*.

![[proc-mining-bpmn.png]]

## Event-Driven Process Chains (EPC)s

## Causal Nets

A **causal net** is a graph where **nodes** represent **activities** and **arcs** represent **causal dependencies.**

Each **activity** has a set of possible **input bindings** and a set of possible **output bindings**.

A Causal net (C-net) is a tuple $C = (A,a_i,a_o,D,I,O)$ where:

- $A \subset \mathcal{A}$ is a finite set of activities
- $a_i \in A$ is the start activity;
- $a_o \in A$ is the end activity;
- $D \subset A \times A$ is the dependency relation,
- $AS = \{X \subset \mathcal{P}(A) | X = \{\emptyset\} \land \emptyset \notin X\}$
- $I \in A \to AS$ defines the set of possible **input bindings** per activity
- $O \in A \to AS$ defines the set of possible **output bindings** per activity

s.t.

- $D = \{(a1,a2) \in A \times A | a_1 \in \bigcup_{as \in I(a_2)} as\}$
- $D = \{(a1,a2) \in A \times A | a_2 \in \bigcup_{as \in O(a_1)} as\}$

### Bindings

The set of **activity bindings:** 

$$B = \{(a,as^I, as^O) \in A \times \mathcal{P}(A) \times \mathcal{P}(A) | as^I \in I(a) \land as^O \in O(a)\} $$

A **binding sequence** $σ$ is a sequence of activity bindings, i.e., $\sigma \in B^*$.

### State

$S = \mathbb{B}(A \times A)$ is the state space of $C$. $s \in S$ is a **state**, i.e., *a multi-set of pending obligations*.

Function $\Psi \in B^* \to S$ is defined inductively: 

1. $\Psi( ) = [ ]$ 
2. $\Psi(\sigma \oplus (a,as^I,as^O)) = (\Psi(\sigma) \backslash (as^I \times \{a\})) \uplus (\{a\} \times as^O)$

for any binding sequence $\sigma \oplus (a,as^I,as^O) \in B^*$. 

$\Psi(\sigma)$ is the state after executing binding sequence $\sigma$.

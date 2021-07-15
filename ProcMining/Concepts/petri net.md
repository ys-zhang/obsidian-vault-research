# Petri Net

## Petri-Net

[https://en.wikipedia.org/wiki/Petri_net](https://en.wikipedia.org/wiki/Petri_net)

A **Petri net**, also known as a **place/transition (PT) net**, is a mathematical modeling languages for the description of *distributed systems*.

A **Petri net** is a ***directed bipartite graph*** that has two types of elements, *places* and *transitions*, depicted as *white circles* and *rectangles*, respectively.



# Simple Petri Net

![[petri-net-simpl.png]]

**configuration**: A Petri net can be initialized by indicating the tokens which are contained in each place at starting time.
**state**: At any time **the distribution of tokens** among places defines the current **state** of the
modeled system.

**Petri net** can be **executed** by firing a **transition** among the set of eligible ones.

### Firing rule: Dynamics of marked Petri Net

A transition is ***enabled*** if each of its input places contains a token.

An ***enabled transition*** can ***fire*** thereby *consuming* one token from each input place and *producing* one token for each output place.

![[petri-net-fire.png]]

### Transition Labels

anecdote **transitions** with **activity labels** 

$$\ell: T \to \mathcal{A}$$

add some more **activity** info on transition $t$, e.g. 
$\ell(t) =\tau$ meaning the transition is *unobservable*.

### Reachable graph

the corresponding *transition system* of the Petri Net.

If the reachability graph is infinite, one can resort to the so-called **coverability graph** that presents a kind of over-approximation of the state space.

## Formal Definitions

**Definition 1.** A **net** is a tuple $N = (P, T, F)$ where 

1. $P$ and $T$ are disjoint finite sets of ***places*** and ***transitions***, respectively.
2. $F = I \cup O$  is a set of (directed) *arcs* (or flow relations).
3. $I \subset P \times T$    **Input arcs**: *Places* → *Transitions*
4. $O \subset  T \times P$  **Output arcs**: *Transitions → Places*

**Definition 2.** Given a net $N = (P, T, F)$, a **configuration** is a set $C$ so that $C ⊆ P$.

**Definition 3.** An **elementary net** is a net of the form $EN=(N,C)$ where:

1. $N = (P, T, F)$ is a net.
2. $C$ is such that  $C \subset P$  is a *configuration*

**Definition 4**. A **Petri net** is a net of the form $PN = (N, M, W)$, which extends the elementary net so that:

1. $N = (P, T, F)$ is a net.
2. $M : P → \mathbb{Z}$ is a *place multiset*, where $Z$ is a countable set. $M$ extends the concept of configuration and is commonly described with reference to Petri net diagrams as a marking.
3. $W : F → \mathbb{Z}$ is an *arc multiset*, so that the count (or weight) for each arc is a measure of the arc multiplicity.



# Variations

## Colored Petri Net

Colored nets are extended Petri nets in which tokens are differentiated by "COLORS".

**Colored Petri nets (CPN)** are the most widely used Petri-net based formalism that can deal with **data-related and time-related aspects** [82, 149].

Tokens in a CPN carry a data value and have a timestamp.

**Transition eligibility** depends then on the **availability of an appropriately colored token in all the input places of this transition.**

Similarly, the output of a transition is not just a token but a specifically colored token.


## Timed and Stochastic Nets

**Timed nets** are *Petri nets* that attach **delays** to **transitions** to give them the ability to
*model time*.
**Stochastic nets** are *Petri nets* that attach **delays** to **places**.

## Capacity of nodes

- **Arcs** have capacity 1 by default; if other than 1, the capacity is marked on the arc.
- **Places** have *infinite* capacity by default
- **Transitions** have no capacity, and cannot store tokens at all

![[vector-id-petri-net.png]]

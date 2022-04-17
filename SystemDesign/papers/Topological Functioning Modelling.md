
# Topological Modelling for Model-Driven Domain Analysis and Software Development

## Problems

 
In software development during the so called _problem domain analysis_ mostly **informal** approaches and languages are used.

**Need formal language for domain analysis using models.** candidate can be:
1. [[petri net]];
2. Topological Model of System Functioning (TFM)
  - The _functional_ properties are cause-effect relations, cycle structure, inputs, and outputs. 
  - The _topological_ properties are connectedness, closure, neighbourhood, and continuous mapping.

## Concepts & Definition

##### Topology and Open Sets
1. $X \in \mathscr O$
2. $\emptyset \in \mathscr O$
3. (finite intersection) $\forall \{O_i\}_{i=1}^N \subset \mathscr O$ , $\bigcap_{i=1}^N O \in \mathscr O$
4. (infinite union) $\forall I \subset \mathscr O$, $\bigcup_{O\in I}O \in \mathscr O$.

#### TFM
A _topological model of system functioning_ can be represented in the form of the topological space $(X, Q)$, where $X$ is a finite set of _functional features_ of the system under consideration, and $Q$ is topology in the form of a _directed graph_.

An _abstract topological model of system functioning_ **visually** can be represented as a directed graph $G(X, U)$
1. $X = N \cup M$ is a _finite closed set of elements_ with some certain topology $Q$ among them.
   - $N$ is [[#Functional Feature]] of the system itself;
   - $M$ is features of other systems.
2. $U$ is a _set of arcs_ that illustrates this topology.

#### Functional Feature as Elements 

>  “Functional” means designed for or capable of a particular function or a use. This means that a functional feature is a characteristic of the system (in its general sense) that is _designed_ and _necessary_ to _achieve some system’s goal_.

 

Each functional feature is a unique tuple
$$\langle A, R, O, PrCond, PostCond, Pr, Ex\rangle$$where
- $A$ is an _action_ linked with an _object_;
- $R$ is a _result_ of that action;
- $O$ is an _object_ related to the action $A$, e.g., role, time period, catalogues, etc.
- $PrCond$ is a set of _precondition_ or an atomic business rule;
- $PostCond$ is a set of _postcondition_ or an atomic business rule;
- $Pr$ is a set of _responsible entities (system or subsystem)_ that provide or suggest an action.
- $Ex$ is a set of _responsible entities_ that enact a concrete action.

Examples:

    <action>-ing the < result> [to, into, in, by, of, from] a(n) <object>

- “evaluating the condition of a print” contains description of the action “evaluate”, result “condition”, and object “print”.
- “servicing a reader” indicates at the action “service” and the object of this action “reader”.


#### Cause-effect Relation as Topology

 > A _cause-and-effect relation_ between two _functional features_ of the system exists if the appearance of one feature is caused by the appearance of the other feature without participation of any third (intermediary) feature.

It is characterised by the nature or business rules not by logic rules. They are such concepts as _ontological necessity_, _probability_ etc.

Cause-and-effect relations have a time dimension, since a cause chronologically precedes and generates an effect.

Practically, studying topological and functional properties of topological models of system functioning (discussed below) reduces to checking on those properties of the according topological digraph.

## Application and Property

>[!WARNING]
>What's the difference with [[PDDR]]?


Continuous mapping states that direction of topological model arcs must be kept as in a refined as in a simplified model.

> If some more detailed functioning system is formed by substitution of a subset of specialised properties for some functional property, then continuous mapping exists between a detailed model and a simplified parent topological model of the same system.


###  MODEL-DRIVEN SOFTWARE DEVELOPMENT WITH TOPOLOGICAL MODELING


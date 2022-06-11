# Reading Notes

 - The most essential notions to understand the rest of the Coq manual: 
     - `term`s and `type`s on the one hand,
     - `command`s and `tactic`s on the other hand.
- `term`s can represent 
    - mathematical _expressions_, _propositions_ and _proofs_, 
    - but also executable _programs_ and program _types_. 
-  A `term` needs an associated `type`. _All types are terms, but not all terms are types._
    - We say that a type is **inhabited** if it contains at least one term (i.e. if we can find a term which is associated with this type). We call such terms **witnesses**.
    - `Coq` is based on the **Calculus of Inductive Constructions**, which is a particular instance of **type theory**(Set theory).
- Coq `document`s are made of a series of `sentence`s that contain `command`s or `tactic`s, generally _terminated with a period_ and optionally decorated with `attribute`s.
  - An `attribute` modifies the behaviour of a _single_ `sentence`.
- A `command` can be used to _modify the state of a Coq `document`_, for instance by declaring a new object, or to get information about the current state.
- A `tactic` specifies how to transform the current **proof state** as a step in creating a proof.
    - They are syntactically valid only when Coq is in [proof mode](https://coq.inria.fr/distrib/current/refman/proofs/writing-proofs/proof-mode.html#term-proof-mode), such as after a [`Theorem`](https://coq.inria.fr/distrib/current/refman/language/core/definitions.html#coq:cmd.Theorem "Theorem") command and before any subsequent proof-terminating command such as [`Qed`](https://coq.inria.fr/distrib/current/refman/proofs/writing-proofs/proof-mode.html#coq:cmd.Qed "Qed").




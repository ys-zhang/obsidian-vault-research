#DSL  #formal-method #verification

We demonstrate a technique to **eliminate the manual proof burden** for _verifying many properties within an entire class of applications_, in our case _reactive systems_, while only expending effort comparable to the manual verification of a single system.

A crucial insight of our approach is simultaneously designing both 
1. a domain-specific language (DSL) for _expressing reactive systems and their correctness properties_ and 
2. proof automation which _exploits the constrained language_ of both _programs and properties_ to enable fully automatic, pushbutton verification.

We dub this design methodology **Language and Automation Co-design (LAC)**.

LAC _restricts both the structure of programs and properties_ to gain much better traction on proof automation to support automatically construction of foundational Coq proofs for applications written in the DSL. Modifying such applications does not create any additional proof burden since the verification is carried out fully automatically.

# REFLEX Language

![[Screen Shot 2022-10-22 at 7.16.11 pm.png]]

- code executes through the _interpreter_.
- behavioural abstraction is generated from source _code_
- auto generate _proof_ from behaviour to correctness _properties_
- one-for-all manual proof is needed for all possible _executions_ satisfies behaviour.


Some other concepts:

- a _trace_ records _all observable interactions_ (from kernels perspective) between the kernel and the outside world.
- _observable interactions_ are also dubbed as _actions_ (these are just messages)
- _property_ is expressed using predicates over _actions_ called __action patterns__, a primitive of the policy language for REFLEX called _Enables_, and _universally quantified_ variables.



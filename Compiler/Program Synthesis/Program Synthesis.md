#compiler  #DSL #program-synthesis

[course-homepage](https://people.csail.mit.edu/asolar/SynthesisCourse/TOC.htm)

# Introduction and Definitions

> **Program Synthesis** correspond to a class of techniques that are able to generate a program from a collection of artefacts that establish _semantic_ and _syntactic_ requirements for the generated code.

1. we expect the synthesiser to produce code that solves our problem, as opposed to relying on extensive search at runtime to find a solution for a particular input, as logic programming systems do. 
2. supporting specification of both semantic and syntactic requirements. We expect synthesis algorithms to provide us with some control over the space of programs that are going to be considered, not just their intended behaviour.

These requirements imply that our synthesis procedures will rely on some form of _search_, although the success of synthesis will be largely driven by our ability to avoid having to exhaustively search the exponentially large space of programs that arise for even relatively simple synthesis problems.

## Applications

- _Software engineering aid_: Copilot, help developers to use complex APIs.
- _Non-expert programming_: help people dealing with small scale programming tasks which they may not even recognised as small scale programming tasks.
- _Reverse engineering of code_: 
    - starting from an implementation, the goal is to infer a specification that characterises the behaviour of the given implementation.
    - creation of models of complex code for the purpose of program analysis.
- a form of interpretable machine learning.

## Challenges

1. _Intention_: how do user tell you their goals?
2. _Invention_: from intention to algorithm
3. _Adaptation_: from algorithm to code that needs to be "optimal" in some sense.
## Publications: 

- programming systems conferences (PLDI, POPL, OOPSLA)
- formal methods (CAV, TACAS) 
- machine learning (NeurIPS, ICLR, ICML).


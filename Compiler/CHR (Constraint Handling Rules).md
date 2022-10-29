#logic  #DSL 

CHR embeds the essential aspects of many _rule-based_ and _logic-based_ formalisms and can implement algorithms in a declarative yet highly effective way. 

The combination of _information propagation_ and _multi-set transformation of relations_ in a concurrent, constraint-based language makes CHR a powerful declarative tool for _knowledge representation_ and _reasoning_.

>[!NOTE] Essential Concepts
> CHR relies on three essential concepts: _rules_, _declaratively_, and _constraints_.


# Rules

Kind of rules:

1. _Simplification rules_ **replace** constraints by simpler constraints while preserving logical equivalence, e.g. $X\le Y \land Y\le X \iff X=Y$.
2. _Propagation rules_ **add** new constraints that are logically redundant but may cause further simplification, e.g. $X\le Y\land Y\le Z \implies X≤Z$.
3. _Simpagation rules_, add new rule and removes **only part of** the constrains and keeps the others. 

>[!NOTE] From view of Logic/Calculus
> Given the transformation rules for deduction in a calculus, 
> - its _inference rules_ map to _propagation rules_ and
> - its _replacement rules_ to _simplification rules_.


# Execution



# Implementation

[SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=chr)


# Problem

> Decision problems in dynamic business context where multiple facts of domain knowledge (conditions, values and goals) frequently change over time, and users should participate continuously in the problem definition.


# Observation

The _effectiveness_ of DSL actually depends on the _degree of correspondence_ between the **subject area** and its **model**: a greater level of _consistency_ results in greater _flexibility_ of the language.

DSL structure contains _semantic_ and _syntactic_ level:
1. _syntactic level_ is responsible for the opportunity to define some _context_;
2. _syntactic level_ of DSL is a result of combination of the general _domain concepts_ with the _problem-specific tasks_ of the users
3. _semantic level_ reflects this _context_ on the concepts of the target domain.
4. (meta-meta-model) _semantic level_ absolutely depends on the model of the target domain, used during DSL creation. 

>[!NOTE]
> Allowing not only to define the tasks in terms of DSL, but also to modify DSL itself should be developed.
>
> It is much more effective to be able to implement **changes** in **DSL** _in parallel with_ **changes** in the **domain**, carrying out their _co-evolution_.


# Background

## Domain Semantic Model (DSM)

$$
DSM = (\langle H_C, H_R \rangle, \langle O, R \rangle, A, M, D )
$$
1. (meta layer) $\langle H_C, H_R \rangle$ **schema** of _class_ and _relation_
2. (object layer) $\langle O, R \rangle$ **instance** of *classes* and *relations*
3. (logical layer) the _rules_, existing in the target domain and different restrictions on them.
    1. $A$: _axioms_ 
    2. $M$: _reasoning_ modules
    3. $D$: _descriptors_ of enabling the recognition of class (concept) instances contained in $O$.
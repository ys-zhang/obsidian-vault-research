
>[!note] difference with evaluation strategy
> This concept is different from [[Evaluation Strategy]].
> _Evaluation strategy_ concerns about when to do reduction (and when to stop reducing), while _reduction strategy_ concerns about how to reduce.

Given a _term rewrite system_, reduction consists of repeating the following 3 steps until a _normal form_ is reached:
1. pick up a _redex_ from the set of all redexes in the term graph/tree;
2. select a rewrite/reduction rule and perform one step of rewrite/reduction;
3. update the root of the selected redex with the result from step 2.

In step 2, usually there is only one legitimate reduction rule, if not there is probably a way to make an equivalent set of new rules that there is no overlaps.

>[!tldr]
>Step $1$, i.e. the strategy of choosing the next redex to reduce, is what the reduction strategy is all about.

>[!note]
> There is a hidden issue not shown in the 3 step, that is the when shall we reduce the redex, this is what **[[Evaluation Strategy]]** concerns.

# normal order reduction

In the context of _lambda calculus_, _normal order reduction_ is the strategy choosing the **leftmost-outmost** redux to reduce, which corresponding to a _pre-order_ traversal of the AST. 

>[!theorem] standardisation 
>If the term has a _normal form_ then _leftmost-outmost_ reduction will eventually (in finite steps) reach it.

# applicative order reduction

_Applicative order reduction_ selects the **leftmost-innermost** redex to reduce, which corresponding to a _post-order_ traversal

>[!warning]
> Even if a normal form exists, _applicative order reduction_ may not terminate.

# weak reducion



# Misc



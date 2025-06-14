
>[!def] structure recursion
> given a function 
> $$ f : A \to B $$
> $f$ is _structure recursive_ if it decompose its input $a:A$ into multiple _immediate smaller_ $a$ according to the _structure_ of type $A$, then recursively solve these subproblems. 



>[!def] generative recursion
> given a function 
> $$ f : A \to B $$
> $f$ is _generative recursive_ if it generates smaller problems from its input using not $A$'s structure information but some other means.
> 
> The key to designing algorithms is the “generation” step, which often means dividing up the problem. And figuring out a novel way of dividing a problem requires insight.

#program-synthesis 


# PBE & PBD

- _Programming by Example (PBE)_: 
    `factorial(6)=720`
- _Programming by Demonstration (PBD)_:
    `factorial(6)=6∗(5∗(4∗(3∗(2∗1))))=720`

Key issues in PBE/PDB:
1. _How do you find a program that matches the observations?_
2. _How do you know the program you found is the one you were actually looking for?_ (restricting search space)


# Bottom Up Explicit Search

```
Synthesize(inputs, outputs):
  plist := set of all terminals
  while(true):
    plist := grow(plist);
    plist := elimEquvalents(plist, inputs);
    forall( p in plist )
      if(isCorrect(p, inputs, outputs)): 
        return p;
```

A key idea behind this algorithm is that the check of equivalence is not an real equivalence check, which would be expensive. 

Instead, the expressions are tested on the target inputs, and any two expression that produce the same outputs on these inputs are deemed equivalent, regardless of whether they are truly equivalent or not. This is what is referred to as _"observational equivalence"_.

This algorithm as described is extremely simple, but it is already quite powerful. 
1. It naturally explores small programs before large programs, so it automatically finds the smallest program satisfying the specification.
2. it is easy to introduce heuristics into `grow` and `elimEquivalents` to direct the search for programs so that programs that are deemed more desirable are discovered first, or to speed up the search based on prior knowledge of which programs are more likely to be correct.
3. it works with black-box language building blocks.
4. The algorithm also does a good job of coping with symmetries, and is able to exploit properties of the building blocks even without access to their source code

## Requirements for correctness

The semantics of a program fragment should not depend on the context.

>[!NOTE] Context Independent Equivalence
> Given two expr $e_1$ and $e_2$ evaluated on a set of inputs $\sigma$, _bottom up search_ requires that:
> $$ 
> \begin{align}
> \forall \sigma. \text{ObsEquiv}(e_1, e_2, \sigma) \implies
> \forall C. \; \text{ObsEquiv}(C[e_1], C[e_2], \sigma)
> \end{align}
> $$
> where $ObsEquiv$ is the observational equivalence of two expressions under a given set of inputs


# (STUN) Synthesis Through Unification

> Rather than trying to synthesise a program that works for all inputs in one shot, one can search for multiple programs that work for different situations and then find a way of combining them together into a program that works for all inputs.

![[STUN.excalidraw|100%]]

 The crucial decision of whether to try to continue to refine the current solution, or to recursively call the STUN procedure is handled by a simple heuristic: _pick a random input, if that input fails, use that input to search for a better solution, if it succeeds, then perform the recursive call_. 
 
 This is a crude heuristic based on the intuition that the recursive call only happens when the current solution already works for a high-enough fraction of the current inputs.


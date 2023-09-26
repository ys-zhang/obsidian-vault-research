#order-theory #set-theory 

# CPO

Let function $F: D \to D$ be any [[CPO (Complete Partial Order)#Continuous|continuous]] function on a [[Poset#Pointed Poset|pointed CPO]] $D$.

Then $F$ has a _least fix-point_ 
$$
 fix(F) = \bigsqcup_n F^n(\bot).  
$$

1. By _monotonicity_ the set $\bot, F(\bot), F^2(\bot),\dots, F^n(\bot), \dots$ is a [[CPO (Complete Partial Order)#Chain-Complete Partial Order|chain]];
2. By _CPO_, $\sqcup_n F^n(\bot)$ exists;
3. By _continuity_, $fix(F)$ is preserved under $F$.


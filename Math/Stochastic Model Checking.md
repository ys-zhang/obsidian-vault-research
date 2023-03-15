#model-checking 

- _Stochastic Model Checking_ by Marta Kwiatkowska, Gethin Norman and David Parker.


> [!PROBLEM]
> How to verify [[DTMC (discrete-time Markov chain)|DTMC]]s and [[CTMC (continues-time markov chain)|CTMC]]s against specifications written in _probabilistic extensions of temporal logic_.


Stochastic model checking is a method for calculating the _likelihood of the occurrence_ of certain events during the execution of a system.

- [[PCTL (Probabilistic Computation Tree Logic)]] is a specification language for DTMC
- _CSL (Continuous Stochastic Logic)_ is a specification language for CTMC


# Model Checking in PCTL for DTMC

![[PCTL (Probabilistic Computation Tree Logic)]]

Define the 

```haskell
data StateFormula = 
  | True 
  | Atom (State -> Bool)
  | Neg StateFormula
  | And StateFormula StateFormula
  | Prob Real PathFormula

data PathFormula = 
  | X StateFormula
  | U Int StateFormula StateFormula
  | UInf StateFormula StateFormula

sat :: StateFormula -> State -> Bool
sat True                 = True
sat (Atom a)             = a
sat (Neg phi)            = not . (sat phi)
sat (And phi psi) s      = (sat phi s) && (sat psi s)
sat (Prob p pathFormula) = ((<=) p) . (prob pathFormula)  -- greater or equal to p

trans :: State -> [(State, Real)]

prob :: PathFormula -> State -> Real
prob (X stateFormula) s = 
  let ss = filter (sat . fst) (trans s)
  in sum $ map snd ss
prob (U k phi psi) s 
  | sat psi s = 1
  | k == 0    = 0
  | sat (And (Neg phi) (Neg psi)) s = 0
  | otherwise = 
      let nexts  = trans s
          probOf = prob (U (k-1) phi psi)
      in 
          prod $ map (\(p, s) -> p * probOf s) nexts
prob (UInf phi psi) s = ... -- solve the linear equation
```

## Adding Rewards

A **reward structure** $(\rho, \tau)$ specifies 2 types of rewards:
1. _state/cumulative_ reward: $\rho: S \to \mathbb R_+$; which are assigned for $1$ step stay at a state $s$.
2. _transition/impulse_ reward: $\tau: S \times S \to \mathbb R_+$; which are assigned for a transition.

**reward extension of state formula**:
$$ 
\begin{align}
  &R_{∼r} [C^{\le k}] \\
  &R_{∼r} [I^{=k}] \\
  &R_{∼r} [F\Phi]
\end{align}
$$
![[Pasted image 20230127185415.png]]

>[!NOTE] Reword Extension Semantics
> - $R_{∼r} [C^{\le k}]$: the expectation of _cumulative reward_ in $k$ steps satisfies the relation `~r`;
> - $R_{∼r} [I^{= k}]$: the expectation of _impulse reward_ at step $k$ satisfies the relation `~r`; 
> - $R_{∼r} [F\Phi]$: the expectation of _cumulative reward_  until $\Phi$ is first time valid satisfies the relation `~r`; 

Expectation of $X_{C^{\le k}}$ and $X_{I^{=k}}$ can be computed by induction on $k$.




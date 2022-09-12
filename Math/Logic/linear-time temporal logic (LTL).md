# Formula BN-form

$$
\begin{align*}
\phi ::= &\;\; \top \;| \; \bot \; | \;p  \\
 &| \; \neg\phi \;|\; \phi \land \phi \;|\; \phi \lor \phi \;|\; \phi \to \phi \\
 &| \; X\phi \\
 &| \; F\phi \; | \; G\phi \\
 &| \; \phi U\phi \; | \; \phi W \phi \;| \; \phi R \phi
\end{align*}
$$
where 
- the first 2 lines gives the propositional logic, and $p$ denotes atoms;
- $X, F, G, U, W, R$ are _temporal connectives_: 
    - $X$ stands for neXt state;
    - $F$ stands for _some_ Future state;
    - $G$ means all future states (Globally);
    - $U$ stands for Until and $W$ is Weak until;
    - $R$ stands for Release.
- $\varphi U \psi$ holds if $\exists i\ge0$ in the future such that $\psi$ holds and $\forall 0\le j < i$ ,$\varphi$ holds.
- $\varphi W \psi \equiv (G\varphi) \lor (\varphi U \psi)$   
- $\varphi R \psi \equiv \neg(\neg\varphi U \neg\psi)$ holds if either
    1. $\exists i \ge 0$ s.t. $\varphi$ holds and $\forall 0\le j \le i$ s.t. $\psi$ holds;
    2. $G\psi$ holds

>[!NOTE] Until
>neither $\varphi U\psi$ or $\varphi W\psi$ implicates anything about $\varphi$ after $\psi$ holds.

Duality:
1. $\land$, $\lor$ are dual to each other;
2. $G$ and $F$ are dual to each other: $\neg G\phi \equiv F\neg \phi$, $\neg F\phi \equiv G\neg \phi$
3. $U$ and $R$ are dual to each other:
    - $\neg(\phi U \psi) \equiv \neg\phi R \neg\psi$
    - $\neg(\phi R \psi) \equiv \neg\phi U \neg\psi$

Distributivity:
1. $F$ distributes over $\lor$: $F(\phi \lor \psi)\equiv F\phi \lor F\psi$
2. $G$ distributes over $\land$: $G(\phi \land \psi)\equiv G\phi \land F\psi$

Expansion:
- $\phi U \psi \implies \psi \lor (\phi \land X(\phi U \psi) )$ ;
- $\phi R \psi \implies \psi \land (\phi \lor X(\phi R \psi))$ .


# Compile a LTL formula to a  [[Buchi Automaton (BA)|GBA]] 

https://en.wikipedia.org/wiki/Linear_temporal_logic_to_Büchi_automaton

## Closure, Cover and Elementary Set

The _closure_ $\text{cl}(\varphi)$ of a LRL formula $\varphi$ is a set of sub-formula of $\varphi$ such that if $\psi \in \text{cl}(\varphi)$ then its negation and sub-formulas also in $\text{cl}(\varphi)$.

>[!NOTE]
>The idea is to construct a GBA where 
> - each of its states corresponds to a subset  $M \subset \text{cl}(\varphi)$
> - the GBA has an accepting run starting from $M$ for a word $w$ if and only if   $w$ satisfies every formula in $M$ and violates every formula in $\text{cl}(\varphi) \backslash M$, i.e.,  $w \models \land_{\xi\in M} \xi$ and $w \not \models \lor_{\xi\in \text{cl}(\varphi) \backslash M } \xi$
> 
> Thus, we only consider _elementary subset_ of $\text{cl}(\varphi)$, which are maximal consistent

An _elementary set_ $S$ of formulas with respect to $\text{cl}(\psi)$ is  a subset of $\text{cl}(\psi)$ such that: 
1. _maximal_: for every sub-formula of $\varphi$, either it or the its negation in $S$,  
2. _logical and temporally consistent_: $\land_{\xi\in S}\xi$ is satisfiable.



A _cover_ of a set of formulas $\Psi$ is a set of sets
$$
  \mathcal C = C_0, C_1,\dots
$$
such that 
$$
\bigwedge_{\psi\in\Psi} \psi \equiv \bigvee_{C_i\in \mathcal C} \bigwedge_{\xi\in C_i} \xi
$$
In other words, any computation satisfying the conjunction of the formulas in the set $\Psi$ also satisfies the conjunction of the formulas in at least one of the cover sets $C_i\in C$

## GBA Construction

Let $\text{cs}(\varphi)$ be the set of _elementary subsets_ of $\text{cl}(\varphi)$.
An equivalent GBA of $\varphi$ is 
$$
  A = (\{q_0\} \cup \text{cs}(\varphi), 2^{AP}, \delta, q_0, F)
$$
where $AP$ is the set of _atomic proposition_.
1. (**transition**): $\delta = \Delta_1 \cup \Delta_2$ and notice the elements of the alphabet are subsets of $AP$.
      - $(M, a, M') \in \Delta_1$ if and only if
          - $M' \cap AP \subset a$,
          - $a\subset \{p\in AP: \neg p \notin M' \}$, meaning $a$ **does not contradict** with $M'$
          - $X\xi \in M$  if and only if $\xi \in M'$,
          - $\xi U \eta \in M$ if and only if either
              1.  $\eta \in M$ 
              2. $\xi \in M$ and $\eta U \xi \in M'$
          - $\xi R \eta \in M$ if and only if either
              1. $\xi \land \eta \in M$ 
              2. $\eta \in M$ and $\xi R \eta \in M'$
    - $\Delta_2 = \bigg\{ (q_0, a, M'): \varphi\in M', M'\cup AP \subset a \subset \{ p\in AP: \neg p \notin M' \} \bigg\}$
2.  (**final states**) for all $\xi U\eta \in \text{cl}(\varphi)$ we have 
    $$ \bigg \{ M \in \text{cs}(\varphi): \eta\in M, \text{ or } \neg(\xi U \eta) \in M \bigg\} \in F $$

Need to prove the language of the GBA is exactly the set of paths that satisfies $\varphi$.

Firstly, pick any path $w = a_0, a_1, \dots$  such that $w \models \varphi$.
Observe that 
$$ M_w = \big\{  \xi \in \text{cl}(\varphi): w\models \xi \big\} \in \text{cs}(\varphi) $$
then 
$$
\rho_w  = q_0, M_w, M_{w[1:]}, M_{w[2:]}, \dots
$$
is an accepting run of the GBA over $w$.

Conversely, we need to prove every acceptable run satisfies $\varphi$.
Suppose $\rho = q_0, M_0, M_1, M_2, \dots$, just needs to prove 
$$ \forall i, M_i = M_{w[i:]} $$

## Gerth Algorithm

The algorithm splits in 2 steps. In the 1st step we incrementally construct a directed graph, and in the 2nd step we build a _labeled generalised Buchi automaton(LGBA)_ by defining nodes of the graph as states and edges as transitions.


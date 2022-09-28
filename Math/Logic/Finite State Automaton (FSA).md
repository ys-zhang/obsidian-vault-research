#model-checking 
#logic 

>[!CITE] Etymology
> 
The word “automaton” (the singular of “automata”) comes from ancient Greek αὐτόματος meaning “self-acting”, from the roots αὐτό- (“self”) and -ματος (“thinking, willing”, the root of Latin _mentus_).


# DFA (deterministic finite-state automata)


A **DFA** is a tuple $M=(\Sigma, Q, s, A, \delta)$ where
  - $\Sigma$ is the **input alphabet** which is a _finite_ set 
  - $Q$ is a finite set of **states**
  - $s\in Q$ is the **start state**
  - $A\subset Q$ specifies **accepting states**
  - $\delta: Q\times \Sigma \to Q$ is the **transition function**


Define the $\delta^*: Q\times \Sigma^* \to Q$:
$$
\delta^*(q, w) = \begin{cases}
q & w =\epsilon \\ \\
\delta^*(\delta(q, a), x) & w=a\cdot x
\end{cases}
$$

> Alan Turing used $q_1,q_2,...,q_R$ to refer to states (or “m-configurations”) of a generic Turing machine. Turing may have been mirroring the standard notation $Q$ for configuration spaces in classical mechanics, also of uncertain origin.


# NFA


>  Nondeterministic finite-state automaton, or NFA, is named so because its behaviour is _not uniquely determined_ by the input string.


A **DFA** is a tuple $M=(\Sigma, Q, s, A, \delta)$ where
  - $\Sigma$ is the **input alphabet** which is a _finite_ set 
  - $Q$ is a finite set of **states**
  - $s\in Q$ is the **start state**
  - $A\subset Q$ specifies **accepting states**
  - $\delta: Q\times \Sigma \to \mathcal 2^Q$ is the **transition function**


Define the $\delta^*: Q\times \Sigma^* \to \mathcal 2^Q$:
$$
\delta^*(q, w) = \begin{cases}
\{q\} & w =\epsilon \\ \\
\bigcup_{r\in\delta(q, a)}\delta^*(r, x) & w=a\cdot x
\end{cases}
$$
Notice the only difference is the transition function $\delta$ is nondeterministic.

Unlike DFAs, however, an NFA does not maintain a single current state, but rather **a set of current states**. 

After all symbols have been read, the NFA accepts $w$ if its current state set contains **at least one** accepting state and rejects $w$ otherwise.

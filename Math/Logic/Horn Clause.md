#logic #Prolog 

# Atomic Formula

In _predicate logic_, an _**atomic formula**_ has the shape 
$$
  P(t_1, \dots,t_n)
$$

where $P$ is a _predicate symbol_, and each $t_k$ is a _term_

>[Example]
> $M(x)$, $P(x, f(y))$ are _atomic formula_, but $P(x) \to M(X)$ is not, since it uses the _connective_ "implies".


# Literal

A _**literal**_ is either 
1. an (_positive_) atomic formula, or
2. a _negated_ atomic formula, e.g., $\neg x$

# Clause

A _**clause**_ is a _disjunction_ of _literals_:

$$
  L_1 \lor L_2 \lor \cdots \lor L_n
$$
where $L_i$ are _literals_

A _**Horn clause**_ is a clause with _at most one_ positive literals.

| Horn clause type | Postive literal | Negative literal |
| ---------------- | --------------- | ---------------- |
| definite clause  | $1$             | $\ge 0$          |
| unit clause    | $1$             | $0$              |
| goal  clause     | $0$             | $> 0$            |

In a definite clause,
$$
H \lor \neg G_1 \lor \cdots \lor \neg G_n
$$
is equivalent to 
$$
  H \lor \neg (G_1 \land \cdots \land G_n)
$$
or _semantic_ equivalently to an _implication_
$$
  H \leftarrow (G_1 \land\cdots\land G_n )
$$
or equivalently to a **_rule_** in [[Prolog]]  
```prolog
% H is call the head of the rule/horn-clause 
% Gi are called goals

H :- G1, G2, ... , Gn.   %1) A rule
```

A _unit clause_ in [[Prolog]] is called a _**fact**_.
```prolog
H.          %1) A fact, i.e., a rule without goals 
H :- true.  %2) Equivalent to 1)
```

A _goal clause_ is [[Prolog]] is called a _query_
```prolog
:- G1, ..., Gk.    %1) A query with k goals
?- G1, ..., Gk.    %2) Equivalent to 1), "?" highlights its a question
```

>[!NOTE]
>Coding as theory building; running a program as ask for theory's logical consequences.

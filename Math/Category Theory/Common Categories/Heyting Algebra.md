#category-theory 

A Heyting Algebra is a [[Lattice#Bounded Lattice|Bounded Lattice]] which all [[Exponentials.excalidraw|Exponentials]] exists.

In Heyting Algebra, the exponential object $Y^X$ is denoted by $X \implies Y$  which reads as "implies". 

# Inference Rules

### Implication Rules

##### Introduction of implication

By universal construction of exponential family we have 

$$
\forall Z, Z \land X \le Y
$$
i.e., there exists an arrow from the product of $Z$ and $X$ to $Y$, we have
$$
  Z \le (X \implies Y)
$$
or in category notation $\text{curry} \in \mathbf{Hom}(Z, Y^X)$, i.e., there exists an arrow from $Z$ to the exponential object $Y^X$

This gives the **introduction of implication rule**:

$$
  Z\land X \le Y \vdash Z \le (X\implies Y)
$$
or in propositional logic's syntax


##### Elimination of Implication

By the evaluation arrow, we have
$$
(X\implies Y) \land X \le Y
$$
or in category notation $\text{eval}\in \mathbf{Hom} (Y^X \times X, Y)$.


### Negation Rules

Define the **pseudo-compliment** object of object $X$ as 
$$
\neg X := (X \implies \bot)
$$
Thus we have the rule of **negation elimination**:
$$
\neg X \land X \le \bot
$$

### And Rules

##### Introduction of And

we have 
$$\text{id} : Y\land X \to X\land Y$$
let $Z=Y$ we rewrite the above formula as 

$$
Z \land X \le  X\land Y
$$
by universal construction of exponential object we have $curry: Z \to (X\land Y)^X$ or
$$
Z \le (X \implies X \land Y)
$$
rewrite $Z$ to $Y$ gives 

$$
Y \le (X \implies X \land Y)
$$

##### Elimination of And

By Category Product

$$
\begin{align}
X \land Y \le X \\
X \land Y \le Y
\end{align}
$$

### Or Rules

##### Introduction of Or
by the category coproduct we have

$$
\begin{align}
X \le X \lor Y \\
Y \le X \lor Y
\end{align}
$$

##### Elimination of Or



### True and False Rules

We can think 
1. $\top$ as True, which is the terminal object
2. $\bot$ as False which is the initial object

Thus we have $\forall X \in \mathbf{Obj}(\mathscr H)$

$$
\begin{align}
X \le \top \\
\bot \le X
\end{align}
$$
The point here is **False entails anything**.


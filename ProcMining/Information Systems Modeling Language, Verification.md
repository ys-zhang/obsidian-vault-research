#academic-paper  #process-mining 

# Information Systems Modeling: Language, Verification, and Tool Support

# Problem And Idea

How to model **information** and **process** and reflecting that **information are managed by** **processes.**

An approach for *modeling* and *verification* of information systems that **combines information models and process models** using an automated **theorem prover.**

### Weaponry

1. *structure* and *constrain* of **information** : *set theory* and *first order logic.*
2. **process** (dynamic aspects of the system): *Petri nets with identifiers*.

### Aim

expressive and formal 

- Model **concepts and constraints** that govern the aspects related to the **information** that a system can manage, i.e., *data, and the semantics of the data, that the processes* of the system can manipulate
- model **concepts and constraints** that govern the aspects related to the **dynamic behavior** that a system can exercise, i.e., *processes* that manipulate the information managed by the system.

basically, model information, process and dynamic of process. 

### Concepts?

- An **information system** is an organized collection of **concepts** and **constraints** for *storing*, *manipulating*, and *disseminating* information.
- A **constraint** is a constraint on ***states***.

# Information Systems Modeling Language

## information modeling

An **information model** consists of a set of possible **entity types** and **relations**, which are characterized by finite sequences of entity types, together with a set of conditions, specified in first-order logic on finite sets with equality.

### Definition 3.1 (Information model)

Let $\mathcal{I}$ and $\Lambda$ be a universe of ***identifiers*** and ***labels***.

An information model is a 4-tuple $(E, R, \rho, \Psi)$, where:

- $E \subset \mathcal{P}(\mathcal{I})$ is a finite set of **entity types**;  each entity type is a set of identifiers.
- $R \subset \Lambda$ is a finite set of **relation types**;
- $\rho: R \to E^*$ is a **relation definition function** that maps every **relation type** onto a finite **sequence of entity types** for which it holds that for every $e \in E$ there exists $r \in R$, called the entity relation of $e$, such that $\rho(r) = \langle e \rangle$; and
- $\Psi$ is a **collection of constraints** defined as a formal theory of the first-order logic statements over a collection of predicates that for every $r\in R$ contains a predicate with the domain $\prod_{i=1}^{|\rho(r)|} \rho(r)_i$


> $\rho$ defines the domain of the predicate $\Psi(r)$ associated with $r$.
> $\Psi$ is basically a set of predicates $\rho(r)_1 \times \dots \times \rho(r)_n \to \{0, 1\}$


### Definition 3.2 (Population, Fact)

A **population** of an **information model** $(E, R, \rho, \Psi)$ is a function 
$$
\pi : R \to \mathcal{P}(\bigcup_{n\in\mathbb{N}} \mathcal{I}^n)
$$ 
such that every element in the population is correctly typed, i.e., for every $r \in R$ it holds that

$$ \pi(r) \in \mathcal{P}(\prod_{i=1}^{|\rho(r)|} \rho(r)_i) $$

> An element in $\pi(r)$ is called a **fact**, it can be `true` or `false`.

>A population may invalidate the constraints. Thus, we say that a population $\pi$  is **valid** if it *satisfies all the constraints of the information model*, denoted by $\pi \models \Psi$

By $\Pi(D)$ and $\Lambda(D)$ we denote the set of all possible populations of information model $D$ and the set of all possible valid populations of $D$, respectively.

$\pi$ is the population assignment, assign population/facts to relations. 

### Definition 3.3 (Transaction)

- Let $D = (E, R, \rho, \Psi)$ be an *information model*.
- Let $r \in R$ be a *relation.* 
- Let $v \in \prod_{i=1}^{|\rho(r)|} \rho(r)_i$ be a fact, and let $\pi \in \Pi(D)$ be a *population*. 

An **operation** $o$ is a tuple $o \in O(D)$ with $O(D) = (R \times \{\oplus, \ominus \} × \bigcup_{n\in N} \mathcal{I}^n)$.

- Operation $o = (r, \oplus, v)$ inserts **fact** $v$ into $r$ in $\pi$, i.e., it results in population $\pi' \in \Pi(D)$, denoted by $(D : \pi \overset{r\oplus v}{\longrightarrow} \pi')$,  where

$$\pi' = (\pi \backslash \{(r, \pi(r))\}) \cup \{(r, \pi(r) \cup \{v\})\}$$

- Operation $o = (r, \ominus, v)$ deletes **fact $v$** from $r$ in $\pi$, i.e., it results in population $\pi' \in \Pi(D)$, denoted by $(D : \pi \stackrel{r\ominus v}{\longrightarrow} \pi')$ where 

$$\pi' = (\pi \backslash \{(r, \pi(r))\}) \cup \{(r, \pi(r) \backslash \{v\})\}$$

A **transaction** $s \in (O(D))^*$ is a finite sequence of operations, such that every subsequent operation is performed in a population resulting from the previous operation.

represent CRUD actions on $\pi$.

## Process modeling

Many analysis techniques for processes ignore data, i.e., tokens in places resembling the state of the process are considered to be indistinguishable.

### Definition 3.4 ([[petri net]] with identifiers)
![[vector-id-petri-net.png]]
Let $\Sigma$ denote a **universe of variables.**

A **[[petri net]] with identifiers (PNID)** is a 5-tuple $(P, T, F, \alpha, \beta)$, where:

- $(P, T, F)$ is a **[[petri net]]**, with a set of **places** $P$, a set of **transitions** $T$, such that $P \cap T = \emptyset$,  and **a flow function** $F: ((P \times T) \cup (T \times P)) \to \mathbb{N}$; if for $n, m \in P \cup T, F(n, m) > 0$, an **arc** is drawn from $n$  to $m$;
- $\alpha: P \to \mathbb{N}$ defines the **cardinality** of a place, i.e., *the length of the vector* carried on the tokens residing at that place; its **color** is defined by $C(p) = \mathcal{I}^{\alpha(p)}$;
- $\beta$ defines the **variable vector** for each **arc**, i.e., 
	$$\beta \in \prod_{f\in F} V_f$$
	where $V_{(p,t)} = V_{(t,p)} = \Sigma^{\alpha(p)}$ for $p \in P, t \in T$.

>**cardinality**: dimension of the place, number of tokens the place can hold.
>**color** is just like the type of the place: set of possible identifier vectors that a resident token can bear.
>**variable vector** seems to be the type of the argument. while arcs are arguments.

### Definition 3.5 (Marking)

Given a PNID $N = (P, T, F, \alpha, \beta)$, its **set of all possible markings** is defined as

$$\mathcal{M}(N) = \prod_{p\in P}C(p) \to \mathbb{N} $$

For $m \in \mathcal{M}(N)$, pair $(N, m)$ is a **marked PNID**.

>For each place and each legal `id vector`  the **marking**
>gives the number of tokens in that place carrying that `id vector` 

A **transition** is **enabled** if a *valuation of variables to identifiers* *exists*, such that each input place contains sufficient tokens with vectors of identifiers induced by the instantiated variable vector of the corresponding arc.

The same valuation is used to determine which vectors of identifiers are produced in the output places.

%%A **marking** of a PNID defines for each place *the amount of tokens per vector identifier.* This is more alike to capacity.%%

### Definition 3.6 (Transition firing in a PNID)

Let $(N, m)$ be *a marked PNID* with $N = (P, T, F, \alpha, \beta)$.

Let valuation $\nu : \Sigma \to I$ be an *injective function* that maps each variable to an identifier.

A transition $t ∈ T$ is **enabled** in $(N, m)$, if  $\forall p \in P$,

$$ [\nu(\beta((p, t)))^{F(p,t)}] \le m(p) $$

Its ***firing***, denoted by $(N : m \stackrel{(t,\nu)}{\longrightarrow} m')$, results in a marking $m'$ with

$$ m'(p)+[\nu(\beta((p, t)))^{F(p,t)}] = m(p)+[\nu(\beta((t, p)))^{F(t,p)}] $$

for all places $p \in P$
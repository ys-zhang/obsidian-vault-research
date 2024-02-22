#logic  #markov-random-field #belief-network

# Markov Random Field
> For detail see Murphy Chapter 19 Undirected graphical models (Markov random fields)

Let 
- $\mathbf x = (x_1, \dots, x_n)$ be a vector of random variables.
- $\boldsymbol \phi = (\phi_1(\boldsymbol x), \dots, \phi_m(\boldsymbol x))$, each component of the array is a _potential function_, which captures _domain behaviours_.
- $\boldsymbol w = (w_1,\dots,w_m)$ is a vector of real valued _weights_.
Then a **MRF** is a _probability distribution_ of the form: 
$$
  \Pr(\boldsymbol x) \propto \exp(\boldsymbol w^T \boldsymbol \phi)
$$



# First Order Logic

We can construct a **MRF** from a set of _logical clauses_  $\boldsymbol C = \{ C_1, \dots, C_m \}$.
each of the logical clause $C_j$ has the following disjunction form:
$$
  \bigg ( \bigvee_{i\in I_j^-} \neg x_i \bigg) 
  \bigvee 
  \bigg ( \bigvee_{i\in I_j^+} x_i \bigg)
$$
which is equivalent to 
$$
  \bigwedge_{i\in I_j^-}x_i 
  \implies 
  \bigvee_{i\in I_j^+}x_i 
$$
where $x_i$ are _atomic propositions_.




## MAP Inference

> [!QUESTION] MAP/(MAX SAT) Problem (NP-Hard)
> Find the most probable model satisfies the clause set.

Optimize Objective:
$$
\begin{align}
\arg\max_{\boldsymbol x\in \{0, 1\}^n } \Pr(\boldsymbol x)
&= \arg\max_{\boldsymbol x\in \{0, 1\}^n } \boldsymbol w^T\boldsymbol \phi(\boldsymbol x) \\
&= \arg\max_{\boldsymbol x\in \{0, 1\}^n } \sum_{C_j\in C} w_j \min \big\{ \sum_{i\in I_j^+} x_i + \sum_{i\in I_j^-} (1-x_i), 1 \big\}
\end{align}
$$

## MAX SAT Relaxation for MAP

The idea of this algorithm is to _approximate_ the objective function using _relaxation techniques_.

> The _MAX SAT_ problem is to find a Boolean assignment to a set of variables that maximises some ___total weight of all satisfied clauses____.

Randomised approximation algorithms can be constructed for MAX SAT by _independently rounding each Boolean variable $x_i$ to true with probability $p_i$__.

Since each clause has the form $\land_{I_j^-} x_i \implies \lor_{I_j^+}x_i$, the _expected weighted satisfaction_ $\hat w_j$ of a clause $C_j$ is
$$
\hat w_j = w_j\bigg(1 - \prod_{i\in I_j^-}p_i\prod_{k\in I_j^+}(1-p_k)\bigg)
$$
Then we have the expected total weight
$$
  \hat W(\boldsymbol p) = \sum_j \hat w_j = \sum_{C_j\in \boldsymbol C} w_j\bigg(1 - \prod_{i\in I_j^-}p_i\prod_{k\in I_j^+}(1-p_k)\bigg)
$$
Maximising $\hat W(\boldsymbol p)$ is equivalent to solving the original optimisation problem.
It can be proved by relaxing $x\in \{0, 1\}$ to $x\in [0, 1]$ in the original optimising 
$$ 
\begin{align}
\arg\max_{\boldsymbol y\in [0, 1]^n } \Pr(\boldsymbol y) &= \arg\max_{\boldsymbol y\in [0, 1]^n } \boldsymbol w^T\boldsymbol \phi(\boldsymbol y) \\
&= \arg\max_{\boldsymbol y\in [0, 1]^n} \sum_{C_j\in C} w_j \min \big\{ \sum_{i\in I_j^+} y_i + \sum_{i\in I_j^-} (1-y_i), 1 \big\}
\end{align}
$$
we get the new optimisation problem with solution $y^*$ (by general purpose solver), then by setting
$$
p_i = \frac{y_i^*}{2} + \frac{1}{4}
$$
then 
$$
\hat W(\boldsymbol p) \ge 0.75 \cdot Z^*
$$
where $Z^*$ is the maximal value of _MAX SAT_ problem.

To find out a legitimate settings of the variables $x_i$, a greedy approach can be applied.
Each variable $x_i$ is greedily set to the value that maximises the expected weight over the unassigned variables, conditioned on either possible value of $x_i$ and the previously assigned variables.
$$
\begin{align}
  x_i &= \arg\max_{x=0,1} \hat W(\boldsymbol p_{i+1:n} | \boldsymbol x_{1:i-1}, x_i) 
\end{align}
$$


## Local Consistency Relaxation for MAP

This approach starts by viewing MAP inference as an equivalent optimisation over _marginal probabilities_.

For each potential function $\phi_j$, 
- suppose $\phi_j$ is defined on an ordered subset of the variables/atom propositions $\boldsymbol x_j \subset \boldsymbol x$.  
- Let $\boldsymbol x_j(i)$ be denote the $ith$ variable in the ordered set $\boldsymbol x_j$.
- Let $\pi_j$ be the _marginal distribution_ of $\boldsymbol x_j$.  

Consider the _first order local polytope_ $L$. 
- Let $\boldsymbol \mu = (\mu_1, \dots, \mu_n)$ be a vector of distribution,  where $\mu_i(k)$ is the _marginal probability of $x_i$ in state $k$_ and a state is a measurable set of $x_i$'s domain.
- The _first order local polytope_ $L$ is defined as.
$$ 
  L = \bigg\{ (\boldsymbol \theta, \boldsymbol \mu) \;\bracevert\; \forall i,j,k,\; \sum_{\boldsymbol x_j |x_j(i) = k} \theta_j(\boldsymbol x_j) = \mu_i(k)   \bigg\} 
$$

> $L$ represents a __consistency constrain__ on components of $\boldsymbol \theta$, i.e., _they are marginal distributions of a joint distribution_ 

Then we can approximate the original problem by maximising sum of expectation weights over all legal distributions
$$
\begin{align}
  &\arg\max_{\boldsymbol{(\theta_,\mu)}\in L} 
    \sum_{j=1}^m w_jE_{\theta_j}[\phi(\boldsymbol x_j)]\\
  
  =\;\;& \arg\max_{\boldsymbol{(\theta_,\mu)}\in L}
  \sum_{j=1}^m w_j \sum_{\boldsymbol x_j}\theta_j(\boldsymbol x_j)\phi_j(\boldsymbol x_j)
\end{align}
$$

Its solution gives a upper bound of the original problem.
 

## Lukasiewicz Logic
| Clause     | Truth Value      |
| ---------- | ---------------- |
| $\neg x$   | $1-x$            |
| $x\lor y$  | $\min(x+y, 1)$   |
| $x\land t$ | $\max(x+y-1, 0)$ |


# From MAP problem to Hinge-Loss MRF 

The MAX SAT relaxation, local consistency relaxation and Lukasiewicz Logic are equivalently gives the following optimisation problem

$$
\arg\max_{\boldsymbol y\in [0, 1]^n} \sum_{C_j\in C} w_j \min \big\{ \sum_{i\in I_j^+} y_i + \sum_{i\in I_j^-} (1-y_i), 1 \big\}
$$

1. Notice that  
$$
  \sum_j w_j = 1
$$
we have the equivalent problem
$$
  \arg\min_{\boldsymbol y \in  [0,1]^n} \sum_{j=1}^mw_j\max \bigg\{1-  
  \sum_{i\in I_j^+} y_i - \sum_{i\in I_j^-} (1-y_i), 0
  \bigg\}
$$
2. (Logical clause relaxation), no longer require the potential function $\phi_i$ represents truth value of a logical clause but the _degree of satisfaction_, rewrite objective function to 
$$
\arg\min_{\boldsymbol y \in  [0,1]^n} \sum_{j=1}^mw_j\max \bigg\{\ell_j(\boldsymbol y), 0
  \bigg\}
$$
where $\ell_i$ are _linear/affine_ in $y$.
3. (squared hinge loss) 
$$
\arg\min_{\boldsymbol y \in  [0,1]^n} \sum_{j=1}^mw_j \bigg (\max \big\{\ell_j(\boldsymbol y), 0
  \big\} \bigg )^2
$$
  A piecewise-linear loss function makes MAP inference _“winner take all,”_ in the sense that it is preferable to fully satisfy the most highly weighted objective terms completely before reducing the distance to satisfaction of terms with lower weights.

# Hinge Loss Markov Random Field

1. Let $\boldsymbol \phi = (\phi_1, \dots, \phi_m)$ be a vector of **potential functions**.
  $$
    \phi_j(\boldsymbol y, \boldsymbol x) = (\max\{\ell_j(\boldsymbol y, \boldsymbol x), 0 \})^{p_j}
  $$
  where 
    - $\ell_j$ is _linear/affine_;
    - $p_j \in \{1, 2\}$
2. Let $D$ be the feasible domain of _linear equality/inequality_ constrains, which represents _hard potentials_, i.e. $w=\infty$
  $$
    \begin{align}
      c_k(\boldsymbol y, \boldsymbol x)  &= 0 \\
      c_k(\boldsymbol y, \boldsymbol x ) &\le 0
    \end{align}
  $$
  where 
    - $c_k$ is learning in both $\boldsymbol x$ and $\boldsymbol y$.
3. **Constrained hinge-loss energy function** is defined on constrained domain $D$ as 
  $$
    f_w(\boldsymbol y, \boldsymbol x) = \sum_{j=1}^mw_j\phi_j(\boldsymbol y, \boldsymbol x)
 $$
4. **Hinge-loss random field**  $P$ over random variables $\boldsymbol y$ and _conditioned on_ r.v. $\boldsymbol x$ is a _probability density function_ as defined as 
    1. If $(\boldsymbol y, \boldsymbol x) \notin D$ then 
      $$\Pr(\boldsymbol y|\boldsymbol x)=0$$
    2. Else 
  $$
      \Pr(\boldsymbol y|\boldsymbol x) = \frac{1}{Z(\boldsymbol w, \boldsymbol x)} \exp(-f_w(\boldsymbol y, \boldsymbol x))
  $$
    we have the MAP is equivalent to minimising the _hinge loss energy_.


# Probabilistic Soft Logic

>[!NOTE] 
> PSL allows HL-MRFs to be easily applied to a broad range of structured machine learning problems by defining _templates for potentials and constraints_.

```
Model in PSL => generate potential/constraints => HL-MRF
```

A PSL program is written in a declarative, first-order syntax and _defines a class of HL-MRFs that are parameterised by the input data_.

PSL provides a natural interface to represent hinge-loss potential templates using two types of rules: _logical rules_ and _arithmetic rules_:
1. _Logical rules_ are based on the mapping from logical clauses to hinge-loss potentials.
2. _Arithmetic rules_ provide additional syntax for defining an even wider range of hinge-loss potentials and hard constraints.

```haskell
type PSLProgram = [PSLRule]
data PSLRule l a = LogicRule LogicRule
                 | ArithRule a

data LogicRule = 
    -- a weight and disjunction of a list of atoms
    -- the Bool specifies whether $p_j = 2$ 
    WeightedLogicalRule Float64 Bool [Atom]
    -- these are hard rules, i.e., with weight = infinity
  | UnweightedLogicalRule [Atom]

data ArithRule = 

type Ident = String
data Term c v = Constant c   -- these form the vocabulary
              | Variable Ident
type Predicate = Prodicate 
  { name :: Ident
  , arity :: Int }

type Atom c = 
    Predicate Predicate [Atom] -- length must match arity 
  | Term Term c


grounded :: Atom c -> Bool
grounded (Predicate _ lst) = List.all grounded lst
grounded (Term (Constant _)) = True
grounded _ = False

-- all atoms contained are observed 
type ClosedPredicates = [Predicate]
-- may contain unobserved Atoms
type OpenPredecates = [Predicate]

-- inputs can be feeded into PSLProgram 
-- to generate Hinge-loss Markov Random Field
data Input = 
    Closed ClosedPredicates  
  | Open   OpenPredecates  
  -- all grounded atoms, all elements
  -- must contain a predicate in either 
  -- `Closed` or `Open`
  | Base   [Atom]     -- set of all atomic propositions
  -- maps from `Base` to ([0, 1] or unobserved)
  -- this provides an alternative method to
  -- asign observation result to Atoms
  | Observer  (Atom -> Option<Float64>)
```

##### Example

```lua
-- variable = { constants }
Person    = {"alexis", "bob", "claudia", "david"} 
Professor = {"alexis", "bob"} 
Student   = {"claudia", "david"} 
Subject   = {"computer science", "statistics"}

-- Predicate(Variable, Variable) as (open/closed)
-- need to specify whethe the predicate is closed
Advises(Professor, Student) as (open)
Department(Person, Subject) as (closed) 
EnrolledInClass(Student, Subject, Professor) as (closed)

-- Observations
Advises("alexis", "david") = 1 
Department("alexis", "computer science") = 1 
Department("bob", "computer science") = 1 
Department("claudia", "statistics") = 1 
Department("david", "statistics") = 1
```













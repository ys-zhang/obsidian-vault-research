#model-averaging 
#logic 


# Concepts

_Constraint programming (CP)_ is a declarative formalism that lets users specify conditions a solution must satisfy. Based on that description, a _constraint solver_ can then search for solutions.

#### CSP (constraint satisfaction problem)
- a set $X$ of **variables**, $X = {x_1,\dots,x_n}$
- for each _variable_ $x_i$, a set $D(x_i)$ of _values_ that $x_i$ can assume, which is called the **domain** of $x_i$
- a set of **constraints**, which are _relations_ among variables in $X$, and which can further restrict their _domains_. 

_Constraint programming_ is not restricted to _CLP_: It is possible to embed constraint solvers in other host languages, even if they might not blend in as seamlessly as they do with [[Prolog]].

>[!NOTE] Logic Programming
> _pure logic programming (LP)_ can be regarded as an instance of _constraint solving_, namely as solving constraints over variables whose _domains_ are **Herbrand terms**.


#### CLP(FD)

_Constraint logic programming_ over **finite domains**, denoted as _CLP(FD)_.  This means that all domains are sets of integers, and the available constraints include at least the common arithmetic relations between integer expressions.

- A _CSP_ is called **consistent** if it has a solution.
- An element $v$ of a domain $D(x)$ is said to be _inconsistent with respect to a given CSP_ if there is no solution in which $x$ assumes the value $v$.
- A _domain_ $D(x)$ is called **domain consistent with respect to a constraint $c$** if $D(x)$ contains all valid values of $x$ with respect to $c$, and no proper subset of $D(x)$ contains all valid values.
- A set of integers $D(x)$ is **bounds consistent with respect to a constraint $c$** if it contains all valid values of $x$ with respect to $c$ and it is a subset of the smallest interval of integers that contains all valid values.
- A **constraint propagator** is a method for _filtering inconsistent elements_ from a domain. The process of deterministically ensuring some form of consistency is called **constraint propagation**.
- Systematically trying out (searching) values for variables is called **labelling**. As soon as a variable is _labeled_, _constraint propagation_ is used to further _prune_ the _search space_. 



> [!NOTE] Search and Labelling
> Since we may have constraints over multiple variables, such as $rel(x_1,x_2)$, 
> we may be to try ground values in $D(x_1)$ to make a progress.


# CSP(FD): finite domain constraint programming


## Searching Strategies

Two degrees of freedom in _searching strategy_: 

1. (_instantiation/variable order_): the instantiation order of variables. 
2. (_value order_): the order in which values are tried for each variable.

Heuristic strategies.

Common instantiation strategies:

1. _First Fail_: Instantiating the variables in order of increasing size of domains
2. _Left most_: Instantiate the variables from left to right in the order they occur in the given list.
3. _First Fail Constraint_: Of the variables having smallest domains, one _involved in most constraints_ is instantiated next.
4. _Minimum_: Instantiate a variable whose lower bound is the lowest next.
5. _Maximum_: -   Instantiate a variable whose upper bound is the highest next

Domain searching strategies:

A good strategy is often to instantiate $x$ to a value of its domain which _constrains the remaining variables the least_.

1. _up_: The values of each domain are tried in ascending order.
2. _down_: The values are tried in descending order.


## Solvers

A _CLP(FD)_ **system** is given a logic program. Users run the program by posting a **query**. The textual response of the system is called an **answer**, which can be 

1. a **solution**: a set of bindings of _query variables_ to ground values.
2. an **exception**.
3. **false**, meaning no solution.
4. a **conditional solution**: indicating that there is a solution if some conditions, which are called **residual constraints** or **residual goals**, hold.


Errors:

1. **incomplete**: fails to find a solution that actually exists. 
2. **unsound**: emits an answer that is not a consequence of the given logic program.


```haskell
type Ident = String  -- identifier

-- AttributedVariable store the constraints each variable is involved in
data Variable = Variable Ident Attr [Constraint]

data Attr = 
  Attr { 
    left;
    right;
    spread; 
    dom :: Domain;        -- variable domain
    gs  :: [Propagator];  -- run iff var becomes grounded
    bs  :: [Propagator];  -- run iff any domain boundaries changed
    os  :: [Propagator];  -- run iff domain changed (in any way)
  }    

-- s for state of the propagator
data Propagator s = Propagator s (s -> Attr -> (s, Attr))
```



# References


#type-theory 

Event-B is a __modelling method__ for  _formalising_ and developing 
systems whose components can be modelled as _discrete transition systems_.

Event-B models are organised in terms of two basic constructs: 

1. __contexts__

	The role of the contexts is to _isolate the parameters_ of a formal model and their properties, which are assumed to hold for all instances;
2. __machines__ 

	Encapsulates a transition system with the state specified by a set of variables and transitions modelled by a set of guarded events.


# Contexts

Elements of _Contexts_ include:

1. _carrier sets_;
2. _constants_;
3. _axioms_; 

    Axioms $A(s,c)$ are **presumed** properties of _carrier sets_ and _constants_.
4. _theorems_; 
    
    Theorems $T(s, c)$ are **derived** properties of _carrier sets_ and _constants_.

**Proof obligations** are generated to ensure that the _theorems_ are derivable from previously defined _axioms_.
- (THM)
$$ A(s, c) \vdash T(s, c) $$


# Machines

_Machines_ specify _behavioural_ properties of Event-B models.

When machine $M$ sees context $C$, it has access to $C$’s carrier sets $s$ and constants $c$, to refer to them when modelling; and $C$’s axioms $A(s, c)$ and theorems $T(s, c)$,to use them as assumptions during proving.

Machines M may contain 

1. _variables_, $v$ define the _state_ of a machine  
2. _invariants_, $I(v)$ are _constraints_ of variable.
3. _theorems_, $R(v)$ additional properties of $v$ derivable from $I(v)$
4. _events_, state changes are described by events
5. and a _variant_.


## Events

_Events_  can change the machine _state_ and have the form:
$$
  \forall x \textbf{ when } G(x, v) \textbf{ then }  Q(x, v)
$$
where 

  - $x$ is the _variant_;
  - $v$ is the _variable_, i.e., machine _state_.
  - $G$ is the _guard_ predicate
  - $Q$ is the _action_ associated with the event.

Actions can be: 

$$
\begin{align}
  a &:= E(x, v) \\
  a &:\in E(x, v) \\
  a &:\vdash P(x, v, a')
\end{align}
$$
where 
  - $a$ is some of the variables contained in $v$.
  - $P$ is dubbed _before-after predicate_, relating pre and post action value of $a$ and $x$;
  - The last two forms are _nondeterministic_.

>[!NOTE] 
> actions are performed concurrently, thus the post condition is the conjunction of all $Q$s.


## Proof obligations

- (INV) invariant, post condition implies invariant
$$ 
  K(v') \vdash I(v'),\; \textbf{ i.e. } \; I(v), G(x, v), P(x,v,v') \vdash I(v')
$$
- (FIS) feasibility, 
$$
  I(v), G(x, v) \vdash \exists a' . P(x, v, a') 
$$


# Extensions

_Context extension_ is a mechanism for introducing more _static details_ into an Event-B development.

_Machine refinement_ is a mechanism for introducing details about the _dynamic properties_ of a model. There are two type of machine refinements:
- superposition refinement;
- data refinement.


### Superposition Refinement

```
+---------+
|    V    |  state of machine M
+---------+
     |
     |     
     |       +---------- additional variables
     |       |
     v       v
+---------+-------+
|    V    |   W   |   state of machine N extends M
+---------+-------+
   ^          ^
   |          |
   +-----+----+
         |
        J(V, W) holds, the predicate specifies the 
                the relation of V and W.
```

- machine $N$ extends its state dimension by adding variable $w$;
- $J$ is called _concrete invariant_, specifying relation of additional variable $w$ and the original one $v$

An event $f$ refines event $e$ if $g$ has a _stronger_ guard and post condition.
suppose:
$$
\begin{align}
e &: G \implies Q \\
f &: H \implies R
\end{align}
$$
1. (GRD) guard strengthening
$$ I, J, H \vdash G $$
2. (SIM) simulation
$$ I, J, H, R \vdash Q $$
3. (INV) invariant
$$ I,J, H, R \vdash J' $$

### Data Refinement

```
+---------+
|    V    |  state of machine M
+---------+
     |
     |     
     |<----- (gluing invariants) J(v,w)
     |       
     v       
+---------+
|    W    |   state of machine N extends M
+---------+
```

Simulation rule:
$$
\begin{align}
  &I(v), \\ 
  &J(v,w), \\ 
  &H(y,w), \\
  &R(y, w, w') \\
  \vdash &\\
  &\exists x,v'. G(x,v) \land Q(x,v,v') \land J(v',w')
\end{align}
$$













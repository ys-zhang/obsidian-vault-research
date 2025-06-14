#type-theory 

The simple type theory or lambda calculus with sum studies properties of some _Cartesian closed category_, which are categories with initial/terminal objects and closed under product/coproduct and exponential object.

>[!NOTE] 
>There are huge numbers of interpretations of type theory, however, only the typing rules matter.
This is Adar

# Basic Concepts

- (**type**) The objects in the Cartesian closed category we are interested in is called **Types**,
    1. the $\top$ , $\bot$ type with are terminal and initial objects respectively;
    2. We have some basic types that serves and generative objects
    3.  Other types are generating using basic types by product, coproduct and taking exponential
- (**term**) terms are values of some type, denoted/declared by $a: A$, i.e., $a$ is a term of type $A$, it can we many interpretations base on what category we are interested in, e.g., 
    - $a \in A$ if we are talking about category $Set$;
    - $a$ is a proof of $A$ if we are talking about some _Logic_;
    - $a$ could be an arrow into $A$ from some special object $H$, if we are thinking in _category theory_.
- (**context**) A context $\Gamma$ is a model in the language of Logic, which is a list of declarations of terms/variables
    $$
    \Gamma = \{x_1:A_1, x_2:A_2, \dots, x_n: A_n\}
  $$
  which means there is an arrow to $A_1\times A_2 \times \cdots \times A_n$ in category theory
- (**judgement**) A judgement, alike sequent in logic, is saying something like in the context of $\Gamma$, we have _blabla..._ happens
    $$
    \Gamma \vdash r:R 
  $$ 
# Inference Rules

###### Case Rule
$$
  \over \Gamma,x:A,\Delta \vdash x:A
$$
 in category language we have $\pi: \Gamma \times A \times \Delta \to A$ which is the projection arrow from product category of object $A$ to $A$.

###### Weak Rule

$$
\Gamma,\Delta \vdash t:T \over \Gamma,x:A,\Delta \vdash t:T
$$
###### Unit Introduction

$$
\over \Gamma \vdash \text{unit}: \top
$$

###### Product Introduction

$$
\Gamma \vdash a:A, \;\; \Gamma \vdash b:B  \over \Gamma \vdash \langle a, b\rangle: A\times B
$$
this comes from the universal construction of product in a category

###### Product Elimination

$$
\Gamma \vdash p: A\times B  \over \Gamma \vdash \text{fst}(p): A
$$
$$
\Gamma \vdash p: A\times B  \over \Gamma \vdash \text{snd}(p): B
$$

###### Function Introduction

$$
\Gamma, a:A \vdash b:B \over \Gamma \vdash (\lambda x:A).b: A\to B
$$

the premise speaks we have an arrow $H\times A \to B$,  then we get an arrow $H \to (A \to B)$ which is just currying.


###### Function Elimination

$$
\Gamma \vdash f: A\to B, \;\; \Gamma \vdash a:A \over
\Gamma \vdash f(a): B
$$
where $f(a) = eval(f, a)$ and $eval: ((A\to B)\times A) \to B$.

###### Bottom Elimination

$$
\Gamma \vdash e:\bot \over \Gamma \vdash \text{abort}(e): A
$$

###### Sum Introduction

$$
\Gamma \vdash a:A \over \Gamma \vdash \text{inject}(a): A + B
$$
$$
\Gamma \vdash b:B \over \Gamma \vdash \text{inject}(b): A + B
$$

###### Sum Elimination

$$
\Gamma \vdash s: A+B, \; \Gamma, a:A \vdash u:C, \; \Gamma, b:B \vdash v:C
\over
\Gamma \vdash \text{match}(s, x.u, y.v): C
$$


###### Equational Rules

1. reflexivity
2. symmetricity
3. transitionary
4. unit uniqueness: $$\Gamma\vdash u:\top \over u\equiv *$$
5. Product computation $$ \Gamma \vdash a:A, \;\; \Gamma \vdash b:B  \over \Gamma \vdash \text{fst}(\langle a, b\rangle)\equiv a $$ $$ \Gamma \vdash a:A, \;\; \Gamma \vdash b:B  \over \Gamma \vdash \text{snd}(\langle a, b\rangle)\equiv b $$
6. Product uniqueness $$ \Gamma \vdash p:A\times B  \over \Gamma \vdash p \equiv  \langle\text{fst}(p), \text{snd}(p)\rangle $$

###### Function Computation Rule

$$
\Gamma,x:A\vdash b:B, \;\Gamma\vdash a:A \over \Gamma\vdash [\lambda x:A.b](a) \equiv b[a/x]:B
$$



# References

- Chapter 2 of [Practical Foundations of Mathematics (paultaylor.eu)](https://www.paultaylor.eu/~pt/prafm/)
- YouTube Videos
    - [Foundations 5: Intuitionistic Logic and Type Theory]() https://youtu.be/h6rTlirbfRI)

see also 
- [[Dependent Type Theory]]
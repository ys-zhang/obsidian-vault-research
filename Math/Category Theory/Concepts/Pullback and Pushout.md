#category-theory
#topos-theory 

# Pullback


## Idea

The idea of pullback roots in Set Theory where we want to find the solution of 
$$
f(x) - g(y) = 0
$$
i.e. the **implicit equation**, or 

$$
  A = \lbrace (x, y): f(x)=g(y) \rbrace \subset X \times Y
$$

## Definition

The **pullback** of a pair $A \overset{f}{\longrightarrow} C \overset{g}{\longleftarrow} B$ or
$$
\begin{CD}
    @. A \\
    @. @VfVV \\
    B @>>g> C 
\end{CD}
$$
is a [[Limit (category)|Limit]] of the following commutative diagram,
$$
\begin{CD}
X @>\pi_A>> A \\
@V\pi_BVV  @VfVV \\
B @>g>> C
\end{CD}
$$
i.e.,
$$
h = f \circ \pi_A = g \circ \pi_B
$$
which indicates that the diagram is also a _cone_, with $X$ is the cone apex.

<iframe class="quiver-embed" src="https://q.uiver.app/?q=WzAsNSxbMSwxLCJYIl0sWzMsMSwiQSJdLFsxLDMsIkIiXSxbMywzLCJDIl0sWzAsMCwiWSJdLFsxLDMsImYiXSxbMiwzLCJnIiwyXSxbMCwxLCJcXHBpX0EiXSxbMCwyLCJcXHBpX0IiLDJdLFswLDMsImgiLDFdLFs0LDEsInBfQSIsMCx7ImN1cnZlIjotMn1dLFs0LDIsInBfQiIsMix7ImN1cnZlIjoyfV0sWzQsMCwiayJdXQ==&embed" width="560" height="560" style="border-radius: 8px; border: none;"></iframe>

- $\pi_A$ is call the _pullback of $g$ along $f$_;
- $\pi_B$ is call the _pullback of $f$ along $g$_;
- the square $XACB$ is called _pullback square_ or _Cartesian square_.
- **pullback** is also called [[#Fibred product In Set|fibred product]].


### Relation with Product

For any category with a terminal object $\top$ :
$$
\begin{CD}
X\times Y @>\pi_X>> X \\
@V\pi_YVV @VVV \\
Y @>>> \top
\end{CD}
$$


### Pullback Lemma

$$
\begin{CD}
  \bullet @>>> \bullet @>>> \bullet \\
  @VVV @VVV @VVV \\
  \bullet @>>> \bullet @>>> \bullet
\end{CD}
$$
If the 2 small squares are pullbacks then the large rectangle is also a pullback.


### Relation with Monomorphism

- An arrow $f$ is [[Morphism#Monomorphism Injection|monic]] if and only if the following diagram is a _pullback_.
        $$
        \begin{CD}
          X @>\mathtt{id}>> X \\
          @V\mathtt{id}VV @VVfV \\
          X @>>f> Y
        \end{CD}
      $$
  - Given the following _pullback_
        $$
        \begin{CD}
          C @>g>> D \\
          @VVV @VVV \\
          A @>>f> B
        \end{CD}
      $$
        then 
        $$f \text{ is monic} \implies g \text{ is monic}$$



## Example

### Fibred product in Set

In Set Theory, the _pullback_ is denoted as $A \underset{C}{\times} B \subset A \times B$, and 
$$
A \mathop{\times}_C B = \{(a, b): f(a) = g(b) \}
$$

Notice
$$
  A \mathop{\times}_C B = \bigcup_{c\in C} \left( f^{-1}(c) \times g^{-1}(c) \right )
$$
where each
  - $f^{-1}(c)$ is a _fibre_ of $f$ 
  - $g^{-1}(c)$ is a _fibre_ of $g$.


### Inverse Image

$$
\begin{CD}
f^{-1}(C) @>f^*>> C \subset \mathtt{Img}(f) \\
@ViVV @VVjV \\
A @>>f> B
\end{CD}
$$
where 
- $i$ and $j$ are _embedding_ 
- the inverse image of $C$ under $f$ arises by pulling $C$ back along $f$


### Kernel Relation

Given a function in $Set$
$$
  f: A \to B
$$
a **kernel relation** $R_f$ can be defined by 
$$
   R_f(x, y) \;\; \mathtt{holds}  \iff f(x) = f(y)
$$
or 
$$
\begin{CD}
R_f @>\pi_1>> A \\
@V\pi_2VV @VVfV \\
A @>>f> B
\end{CD}
$$

Notice

$$
  R_f = \bigcup_{b\in B} \left( f^{-1}(b) \times f^{-1}(b) \right )
$$
where each $f^{-1}(b)$ is a fibre of $f$.


# Pushout

**pushout** is the dual of pullback, $B \overset{f}{\longleftarrow} A \overset{g}{\longrightarrow} C$ or
$$
\begin{CD}
A  @>f>> B \\
@VgVV \\
C 
\end{CD}
$$
which is a colimit of 
$$
\begin{CD}
A  @>f>> B \\
@VgVV @VVi_BV\\
C  @>>i_C> Y
\end{CD}  
$$
while 
  - $i_C$ is the pushout of $f$ along $g$;
  - $i_B$ is the pushout of $g$ along $f$.


### Relation with Sum

For any category with a initial object $\bot$ :
$$
\begin{CD}
\bot @>>> X \\
@VVV @VViV \\
Y @>>j> X + Y
\end{CD}
$$


### Pushout Lemma

$$
\begin{CD}
  \bullet @>>> \bullet @>>> \bullet \\
  @VVV @VVV @VVV \\
  \bullet @>>> \bullet @>>> \bullet
\end{CD}
$$
If the 2 small squares are pushouts then the large rectangle is also a pushout.


### Relation with Epimorphism

- An arrow $f$ is [[Morphism#Epimorphism Surjection|epic]] if and only if the following diagram is a _pushout_.
        $$
        \begin{CD}
          X @>f>> Y \\
          @VfVV @VV\mathtt{id}V \\
          Y @>>\mathtt{id}> Y
        \end{CD}
      $$
  - Given the following _pushout_
        $$
        \begin{CD}
          A @>f>> B \\
          @VVV @VVV \\
          C @>>g> D
        \end{CD}
      $$
        then 
        $$f \text{ is epic} \implies g \text{ is epic}$$



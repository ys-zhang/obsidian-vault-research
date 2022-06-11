## Subobject

Consider a category $\mathscr C$ and fix an object $D$, a **subobject** of $D$  is a [[Morphism#Monomorphism Injection|monic arrow]] points to $D$,
$$
f: X \hookrightarrow D
$$

The subobjects of object $D$ is the collection
$$
\text{Sub}(D) := \{[f] : f \text{ is monic with codomain } D\}
$$
where $[f]$ means the equivalent class defined by reflexive [[#Inclusion Relation]] 


## Inclusion Relation

Given a subobject $f:A \rightarrowtail D$ and $g: B \rightarrowtail D$, we say $f$ is **included** in $g$, or $f\subset g$ if and only if $\exists h: A \to B$ such that $$ f = g\circ h $$
i.e., we have the following commutative diagram:
![[subobj-incl.excalidraw|center]]
in addition, $h$ must be a _monomorphism_ since $f$ is monic.
>[!PROOF]
> To prove $h$ is monic we just need to prove 
> $$ (\forall \phi,\varphi, \; h\circ \phi = h\circ \varphi) \implies \phi = \varphi $$
> which is obvious if we compose $g$ on both LHS and RHS. 

The _inclusion relation_ is 
1. reflexive 
2. transitive
3. furthermore, antisymmetric up to _isomorphism_ 
        ![[subobj-incl-antisym.excalidraw|center]]
      the above commutative diagram indicates $h$ and $i$ are both isomorphic. Since $f$ is monic(left eliminate), then only need to prove $f\circ (i\circ h) = f\circ id$. 
      Observing that $(f \circ i)\circ h = g\circ h = f = f\circ id$.   


# Elements of subobject

Given $f\subset B$,
$a\in f$ if and only if $\exists b\in B$ and the following diagram commutes:
![[comm-subobj-elem.excalidraw|center]]


# Classifying subobject

the idea of classifying object is that in _Set Theory_, there is a one-to-one correspondence of  $D$'s subobjects and the hom-set $\mathbf{Hom}(D, \{0,1\})$, i.e., _characteristic function_.  

### Definition

In _Category Theory_, if $\mathscr C$ is category with a **terminal object** $1$, a **subobject classifier** for $\mathscr C$ contains
1. An object $\Omega$ 
2. and a morphism $true: 1\to\Omega$.
such that the following **($\Omega$-Axiom)** holds:
For each monomorphism 
$$
  f: A \rightarrowtail D
$$
There is _one and only one_ morphism (**characteristic arrow**)
$$
  \chi_f : D \to \Omega
$$
such that the following diagram is a [[Pullback and Pushout|pullback square]]:
$$
\begin{CD}
A @>f>> D \\
@V!VV @VV\chi_f V \\
1 @>true>> \Omega
\end{CD}
$$

>[!NOTATION] NOTATION: True arrows
>
>Denote $(true \;\circ \;!) \;:\; A \to \Omega$ as $true_A$ or $\top_A$
> ![[subobj-true.excalidraw|center]]
> And $\forall f: A \to B$ we have $true_B \circ f = true_A$ 

>[!NOTE]
>Given a category, _subobject classifiers_ are same up to isomorphism.

>[!THEOREM]
> For $f\subset D$ and $g\subset D$
> $$ f \cong g \iff \chi_f = \chi_g$$
> where the equivalence means up to isomorphism
> Thus
> $$ \mathbf{Sub}(D) \cong \mathbf{Hom}_{\mathscr C}(D, \Omega)  $$


### Truth Values

Elements of the subobject classifier $\Omega$ are called truth values:
$$ \mathbf{Elem}(\Omega) = \mathbf{Hom}_{\mathscr C}(1, \Omega) $$
thus from the above theorem, *Elements of $\Omega$ is isomorphic to subsets of the terminal object*.
$$ \mathbf{Sub}(1) \cong \mathbf{Elem}(\Omega)$$


# Algebra of Subobject

## Truth Arrows
1. (**False**) $\bot: 1 \to\Omega$ is the characteristic morphism of  $!: 0 \to 1$, i.e., the pullback
      $$
      \begin{CD}
      0 @>!>> 1     \\
      @V!VV @VV\bot V    \\
      1 @>>\top> \Omega
      \end{CD}
   $$
2. (**Negation**) $\neg: \Omega \to \Omega$, and the following diagram is a pullback:
      $$
     \begin{CD}
       1 @>\bot>> \Omega \\
       @V!VV   @VV\neg V   \\
       1 @>>\top>  \Omega
     \end{CD}
   $$
3. (**And**) $\cap: \Omega\times\Omega\to\Omega$ is the characteristic morphism of $$\langle \top,\top\rangle: 1 \to \Omega\times\Omega$$
4. (**Or**) $\cup: \Omega\times\Omega\to\Omega$ is the characteristic morphism of $$[\langle \top_\Omega,1_\Omega \rangle, \langle 1_\Omega, \top_\Omega \rangle]: \Omega+\Omega \to \Omega\times\Omega$$   ![[comm-subobj-or.excalidraw|center]]
6. (**Imply**) $\Rightarrow: \Omega\times\Omega\to\Omega$ is the characteristic morphism of $$e:  (\le) \to \Omega\times\Omega$$ where $(\le) = \{\langle 0,0\rangle, \langle 0,1\rangle, \langle 1,1\rangle\}$.



## Subset algebra

1. (**Complement**) $f \subset D$ the **complement** of $f$ relative to $D$, denoted by $-f$ has the following _pullback_
      $$
    \begin{CD}
      -A @>-f>> D \\
      @V!VV @VV\neg\circ\chi_fV \\
      1 @>>\top> \Omega
    \end{CD}
   $$
2. (**Intersection**) given $f, g\subset D$, the **intersection** of $f$ and $g$ denoted by $f \cap g$ has the following _pullback_:  
      $$
      \begin{CD}
      A\cap B @>f\cap g>> D \\
      @V!VV @VV\chi_f\cap\chi_gV      \\
      1 @>>\top> \Omega
      \end{CD}
   $$
      where $\chi_f\cap\chi_g = \cap \circ \langle \chi_f,\chi_g \rangle$.
      Furthermore, we have the following _pullback_ and the arrows are _embeddings_
      $$
      \begin{CD}
        A\cap B @>>> A \\
        @VVV @VVV  \\
        B @>>> D
      \end{CD}
   $$
3. (**Union**) given $f, g\subset D$, the **union** of $f$ and $g$ denoted by $f \cup g$ has the following _pullback_: 
      $$
      \begin{CD}
        A\cup B @>f\cup g>> D  \\
        @V!VV @VV\chi_f\cup\chi_gV   \\
        1 @>>\top> \Omega
      \end{CD}
   $$
      where $\chi_f\cup\chi_g = \cup \circ \langle \chi_f, \chi_g \rangle$





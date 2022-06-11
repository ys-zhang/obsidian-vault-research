#category-theory 
#topos-theory 
#topology

# Elementary Topos

An **elementary topos** is a _category_ $\mathscr E$ such that
1. $\mathscr E$ is finite complete.
2. $\mathscr E$ is finite cocomplete.
3. $\mathscr E$ has [[Exponentials.excalidraw|exponentiation]].
4. $\mathscr E$ has a [[Subobject#Classifying subobject|subobject classifier]].

Since _initial_ is colimit of empty diagram and _terminal_ is limit of empty diagram, thus rule 1 and 2 guarantees existence of _initial_ and _terminal_ object.

>[!NOTE]
> - Rule 1 can be replaced by existence of terminal  and pullback
> - Rule 2 can be replace by existence of initial and pushout 



# Bundles

![[Bundle.excalidraw|800]]


# Theorems

1. (**monic**): If $f: A\to B$ is monic arrow in some topos then $f$ is an equalizer of $\chi_f$ and $true_B$. 
2. (**isomorphism**): Given a topos, an arrow which is both monic and epic is a isomorphism.
3. (**factorization**): Given a topos, all arrows $f$ can be factorized into an epimorphism and a monomorphism, i.e., 
        $$ f = Im(f) \circ f^* $$
        ![[comm-topos-factorization.excalidraw|550|center]]
        if any $u, v$ makes the left diagram commutes then there exists unique $k$ makes the right diagram commutes.
4. (**fundamental theorem**) if $\mathscr E$ is a topos then bundles $\mathscr E(I)$ is a topos for any object $I$ 
5. In topos, pullback preserve epimorphism.
6. In topos, coproduct/sum preserve pullback.


## Extensionality (analogy to Set)

- In a nontrivial category, the initial object $0$ is **empty**, i.e., _no arrows from $1$ to $0$_.

>[!DEF] Extensionality Principle for Arrows
> 
> if $f\ne g$ and $f,g : A \to B$ then exists an element of $A$
> $$a: 1 \to A$$
> such that 
> $$f\circ a \ne g\circ a$$
> A category satisfies _extensionality principle_ is said to be **well pointed**.
> - If $\mathscr E$ is _well pointed_ then _non-zero_ implies **non-empty**.
> - If  $\mathscr E$ is _well pointed_ then $\mathscr E$ is **bivalent**, i.e., there is only $2$ elements $\{\top, \bot\}$ in the subobject classifier $\Omega$, 
>      $$ \begin{CD} 0 @>!>> 1 \\ @V!VV @VV\bot V \\ 1 @>>\top> \Omega \end{CD} $$
> - If  $\mathscr E$ is _well pointed_ then $$[\top, \bot]: 1 + 1 \to \Omega$$ is an isomorphism.
> - A topos is _well pointed_ is it is classical and every non-zero object is non-empty
> - if topos $\mathscr E$ is _well pointed_, then
>     1. an arrow is _epic_ iff it is _surjective_
>     2. an arrow is _monic_ iff it is _injective_ 



# References

Books:
1. Goldblatt, R. (2014). _Topoi: the categorial analysis of logic_. Elsevier. retrieved from https://projecteuclid.org/ebooks/books-by-independent-authors/Topoi-The-Categorial-Analysis-of-Logic/toc/bia/1403013939


Blogs:
- [topos (ucr.edu)](https://math.ucr.edu/home/baez/topos.html)




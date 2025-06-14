
```haskell
import Test.QuickCheck

prop_list :: (forall a . [a] -> [a]) -> (b -> c) -> Property
prop_list r f = forall
  map f . r === r . map f
```

# Parametricity

The idea is to read types as relations. 

>[!def] relation
> A relation $\mathcal A$ is a tripple $(A, A', rel_{\mathcal A})$ where 
> $$ rel_{\mathcal A}: (A, A') \to Bool $$
> let $A \Leftrightarrow A'$ denote all relations defined on the _pair of sets_ $(A, A')$, the notation
> $$
> \mathcal{A} : A \Leftrightarrow A'
> $$
 means $\mathcal A$ is a relation between $A$ and $A'$

let us define an [[General Theory of Representation and Abstraction|representation]] of _types_ as _relations_:

1. (_type_) $repr(A) = \mathcal{I_A}$ where $\mathcal{I_A} : A \Leftrightarrow A$ is defined by 
   $$ x \sim_{\mathcal I_A} y  \iff x \; \mathrm{is} \; y \;$$
2. (_product_) $repr((A, B)) = repr(A) \times repr(B)$ where 
   $$ 
      \begin{align}
        \mathcal{A} &: A \Leftrightarrow A' \\
        \mathcal{B} &: B \Leftrightarrow B' \\
        \mathcal{A \times B} &: (A, B) \Leftrightarrow (A', B')
      \end{align}
     $$
    and 
    $$
     (a, b) \sim_{\mathcal{A \times B}} (a', b') \iff a \sim_{\mathcal A} a' \; \bigwedge \;  b \sim_{\mathcal{B}} b'
    $$
3. (_list_) $repr([A]) = repr(A)^*$ which is a relation between lists
   $$ 
      \begin{align}
        \mathcal{A} &: A \Leftrightarrow A' \\
        \mathcal{A^*} &: A^* \Leftrightarrow (A')^* \\
      \end{align}
     $$
    and 
     $$ 
          s \sim_{\mathcal{A^*}} s' \iff  |s| = |s'| \; \bigwedge \; \forall i, s_i \sim_{\mathcal{A}} s'_i 
       $$
    its just a natural generalisation of relation product
4. (_function_) $repr(A \to B) = repr(A) \leadsto repr(B)$ which is a relation between functions
    $$ 
      \begin{align}
        \mathcal{A} &: A \Leftrightarrow A' \\
        \mathcal{B} &: B \Leftrightarrow B' \\
        \mathcal{A \leadsto B} &: (A \to B) \Leftrightarrow (A'\to B')
      \end{align}
     $$
    and 
    $$
    \begin{align}
      f \sim_{\mathcal{A \leadsto B}} f' & \iff \\ 
       & \forall a\in A, \forall a'\in A', a \sim_{\mathcal{A}} a' \implies f(a) \sim_{\mathcal{B}} f'(a')  
    \end{align}
     $$
5. (_higher rank types_) consider type `forall a. F a`, $repr(F) = \mathcal F$ is a function that _maps relation to relation_. 
    $$repr(\forall A. F(A)) = \forall \mathcal{A}. \; \mathcal F(\mathcal A) $$
    and $\forall \mathcal{A}. \; \mathcal F(\mathcal A)$ is a relation of _polymorphic values_.
    $$
    \begin{align}
      g \sim_{\forall \mathcal{A}. \; \mathcal F(\mathcal A)} g' & \iff \\
      \; & \forall \mathcal A: A\Leftrightarrow A'. g_{A} \sim_{\mathcal{F(A)}} g'_{A'} 
      \end{align}
      $$


>[!proposition] parametricity
> If $t$ is a _closed term_ of _closed type_ $T$, then $t \sim_{\mathcal T} t$, where $\mathcal T = repr(T)$.
> 
> - _closed terms_ are terms without free variables.
> - _closed types_ are types without free type variables

# Examples

## Free Theorem for `forall a . [a] -> [a]`

```haskell

```


# References

1. Hackage `free-theorems`, from https://hackage.haskell.org/package/free-theorems
2. a web instance of `free-theorems` at http://free-theorems.nomeata.de


<iframe src="http://free-theorems.nomeata.de" width="100%" height="450"></iframe>
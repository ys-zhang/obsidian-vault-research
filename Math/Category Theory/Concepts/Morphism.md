#category-theory 

# Monic Arrows

## Monomorphism (Injection)

In Set Theory an injection satisfies:

$$
\forall x,y\in D, f(x) = f(y) \implies x = y
$$
in Category Theory
$$
  f \circ g = f\circ h \implies g = h
$$
i.e., **monomorphism is "left-cancellable"**.


## Epimorphism (Surjection)

In Set Theory a surjection satisfies:

$$
\forall c \in C, \exists d\in D, \; s.t.\; f(d) = c
$$
in Category Theory, 
$$
  g \circ f = h\circ f \implies g = h
$$
i.e., **epimorphism is "right-cancellable"**.


# Isomorphism

An arrow $f : A \to B$ is an **isomorphism** if exists $g: B \to A$ such that
$f\circ g = id_B$ and $g\circ f = id_A$. 

>[!WARNING]
> An arrow which is both a monomorphism and epimorphism does not guarantee to be an isomorphism.
>
>However, if the category is a [[Topos Theory|topos]], then above the proposition is true.


# Extensionality

## Injection

Suppose we have a morphism
$$ f: A \to B $$
$f$ is **injective** if and only if $\forall a_1, a_1 \in A$ , in other words
$$
  a_1, a_2: 1 \to A
$$
we have 
$$
  f \circ a_1 = f\circ a_2 \implies a_1 = a_2
$$


## Surjection

Suppose we have a morphism
$$ f: A \to B $$
f is **surjective** if and only if $\forall b: 1 \to B$, there exists $a: 1 \to A$ satisfies
$$
  f \circ a = b
$$


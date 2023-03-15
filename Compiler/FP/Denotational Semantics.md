#functional-programming  #formal-method #Haskell 

What does a Haskell program mean? This question is answered by the **denotational semantics** of Haskell. 

In general, the _denotational semantics_ of a programming language maps each of its programs to a mathematical object (denotation), that represents the _meaning_ of the program in question.

_Denotational semantics_ cannot answer questions about how long a program takes or how much memory it eats; this is governed by the _evaluation strategy_ which dictates how the computer calculates the normal form of an expression. 

>[!example]
>the _mathematical object_ for the Haskell programs `10`, `9+1`, `2*5` and `sum [1..4]` can be represented by the integer _10_. We say that all those programs **denote** the integer _10_.
>
> The collection of such _mathematical objects_ is called the **semantic domain**.

The mapping from _program code_ to a _semantic domain_ is commonly written down with double square brackets ("Oxford brackets") around program code.

$$
  [\![ \cdot ]\!] : \mathbf{Code} \to \mathbf{Math}
$$
Examples: 
$$ 
 \begin{align}
   [\![ 2 * 5 ]\!] &= 10 \\
   [\![ \mathbf{Integer} ]\!] &= \mathbb Z 
 \end{align}
$$





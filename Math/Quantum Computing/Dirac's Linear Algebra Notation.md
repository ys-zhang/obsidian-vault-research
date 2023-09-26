#quantum-computing

# Vectors: Ket & Bra

- **ket** is _column vector_, usually with the name $v$ and denoted as $|v\rangle$
- **bra** is _row vector_, usually with the name $w$ and denoted as $\langle w|$

Note the inner product can be expressed as

$$
  \langle w | v\rangle = 
  \begin {bmatrix}
   w_1, \dots,w_n
  \end{bmatrix}
  \begin {bmatrix}
   v_1 \\ 
   \vdots \\ 
   v_n
  \end{bmatrix}
  = \sum w_iv_i 
$$
**Norm** or **length** of a vector is denoted by 
$$
  |\langle a|| = ||a\rangle|| =  \sqrt { \langle a | a \rangle } \\
$$
scalar multiplication $c |v\rangle$, and $c\langle w |$.
addition $\langle a + b |$, and $|c + d\rangle$

# Basis


An ordered basis is a basis in which the vectors have been given an order, that is, there is a first vector, a second vector, and so on. 

If $\{ |b_1\rangle, |b_2\rangle ,\dots , |b_n\rangle \}$ is a basis, we will denote the ordered basis by $( |b_1\rangle, |b_2\rangle ,\dots , |b_n\rangle )$ - we change the brackets from curly to round.

$$
\begin{align}
  \langle \uparrow | &= [1, 0]  \\
  \langle \downarrow | &= [0, 1]  \\
  \langle \rightarrow | &= [\frac{1}{\sqrt 2}, \frac{-1}{\sqrt 2}]  \\
  \langle \leftarrow | &= [\frac{1}{\sqrt 2}, \frac{1}{\sqrt 2}]  \\
  \langle \nearrow | &= [\frac{1}{2}, \frac{-\sqrt 3}{2}]  \\
  \langle \swarrow | &= [\frac{\sqrt 3}{2}, \frac{1}{2}]  
\end{align}
$$
The value of each direction is chosen to make 
1. inverse directions have inner product $0$
2. orthogonal directions have inner product $1/2$ (imagine measure spin first in vertical then in horizontal)



# State

Before any measurement, the particle will be in a _spin state_ given by 
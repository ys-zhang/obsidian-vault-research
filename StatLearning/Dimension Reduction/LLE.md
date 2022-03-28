# Local Linear Embedding

## Motivation
> Points nearby should be mapped nearby, while points far away should impose no constraint.
> This is a **local** method as it involves local [[Principle Component Analysis|PCA]] and sparse eigenvector  
decomposition.

## Idea
![[Pasted image 20210726221141.png]]

1. Define what is "local": $\epsilon-$neighbours
2. How to describe local structure: convex combination of neighbourhood.
3. Keep local structure in low-dimensional space.

## Laplacian LLE (Eigenmap)

$$
\min_M \int \|\nabla_M f\|^2, \|f\|=1
$$
minimize manifold curvature.


## Hessian LLE

$$
\min_M \int \|\mathcal{H}_M f\|^2, \|f\|=1
$$
minimize Hessian.

##  Local Tangent Space Alignment (LTSA)

In contrast to Hessian LLE’s minimization of projections on pairwise products between tangent vectors, LTSA minimizes the projection on the normal space.


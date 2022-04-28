#statistics  #general-additive-model #algorithm  #nonparametric-statistics  #summarize 

- [Decision Tree](https://scikit-learn.org/stable/modules/tree.html#)
- [ID3](https://en.wikipedia.org/wiki/ID3_algorithm)

# The Model
Let 
- $T$ denote the partition;
- $|T|$ be the number of regions or leaf nodes  
- $N_m$ be the number of samples in region $R_m$;
- $Q_m$ some loss or criteria in region $R_m$, e.g. MSE:
$$
Q_m(T) = \frac{1}{N_m} \sum_{x_i\in R_m} (y_i - \hat c_m)^2
$$
There are 2 phases in a tree model:
- grow phase
- prune phase

Different algorithms have different methods for grow and prune phase.



## Regression Tree

$$
 f(X) = \sum_{i=1}^M c_m \mathbb I(X\in R_m)
$$
The task is to 
1. determine the partition $R_m$
2. estimate $c_m$ 

$c_m$ is easy to be estimated as $\hat c_m = \text{avg} \{y_i: x_i\in R_m\}$      




## Classification Tree
Estimate 
$$
\Pr(G=k|x\in R_m)
$$
using 

$$
\hat p_{mk} = \frac{1}{N_m} \sum_{x_i\in R_m} \mathbb I(y_i = k)
$$
and the prediction
$$
\hat c_m = \arg \max_k \hat p_{mk}
$$


# Criteria (Loss function)

## Classification Criteria

##### Gini index
$$
\sum_{k\ne k'} \hat p_{mk} \hat p_{mk'} = \sum_{k=1}^K \hat p_{mk} (1-\hat p_{mk})
$$
##### Cross entropy
$$
-\sum_{k=1}^K \hat p_{mk}\log \hat p_{mk}
$$
##### Miss Classification
$$
\sum_{k=1}^K (1- \hat p_{mk})
$$


## Regression Criteria

##### MSE
$$
Q_m(T) = \frac{1}{N_m} \sum_{x_i\in R_m} (y_i - \hat c_m)^2
$$

##### Half Poisson Deviance
$$
Q_m(T) = \frac{1}{N_m} \sum_{x_i\in R_m} (y_i\log\frac{y_i}{\bar y_m} - y_i + \bar y_m)
$$


# CART (Classification and Regression Tree) Algorithm

- [CART](https://online.stat.psu.edu/stat508/lesson/11/11.8/11.8.2)

## Grow Phase

>[!IDEA]
>  The point here is how to choose the right predictor $j$ and then the partition point $s$.
> 
>  1. CART uses a greedy approach by choose a split that minimise some Loss function of the model;
>  2. The splitting process terminates if splitting result in nodes exceed some threshold

$$
\min_{j, s} 
\big\lbrace \;
  \min_{c_1} Q(R^-_{(j,s)}|c_1) 
  + \min_{c_2} Q(R^+_{(j,s)}|c_2) \;
\big\rbrace
$$
where
- $R^-_{(j,s)} = \{x: x_j \le s \}$
- $R^+_{(j,s)} = \{x: x_j > s \}$



## Cost Complexity (Criterion) Pruning

The idea is to find a smallest subtree $T_\alpha \preceq T_\max$ minimises $C_\alpha$:
$$
C_\alpha(T) = \sum_{m=1}^{|T|} N_mQ_m(T) + \alpha|T|
$$
where 
- $|T|$ is the number of leaf nodes in the tree.
- $Q_m$ is the mean loss in leaf node $m$.

>[!Definition]   
>**Smallest Complexity Tree**
>
> 1. $C_\alpha(T_\alpha) = \min_{T\preceq T_\max} C_\alpha(T)$;
> 2. For all $T$ minimises $C_\alpha$, $T_\alpha \preceq T$.

Properties of the result:
- $T_\alpha$ exists and unique;
- $T_{\alpha}$ is nested, $\forall \alpha_1 \ge \alpha_2, T_{\alpha_1}\preceq T_{\alpha_2}$  


##### Weakest Line Pruning

The **algorithm** to find $T_\alpha$ is dubbed as **Weakest Line Pruning**: 

1. For each leaf note $t\in T$ we define the **link** $\alpha_t$ as the smallest $\alpha$ such that $C_\alpha(t) \le C_\alpha(T_t)$ where $T_t$ is the branch connects the root and $t$;
2. The leaf node $t$ with the smallest $\alpha_t$ is called the **weakest link**;
3. At the $k$th iteration, approximate function of $t \to \alpha_t$ by
    $$
    g_{k}(t) = \begin{cases}
      \frac{C_0(t) - C_0(T_t)}{|T_{\alpha_k}| - 1} & \forall t\in \text{Leaf}(T_{\alpha_k}) \\
      +\infty  & \text{otherwise}
    \end{cases}    
  $$
      1. Let $\alpha_{k+1}=\min_t g_k(t)$ and $t_{k} = \arg\min_t g_k(t)$
      2. Let $T_{\alpha_{k+1}}$ to be $T_{\alpha_k}$ prune the branch of $t_k$


# ID3 (Iterative Dichotomiser 3)

- [a blog](https://towardsdatascience.com/decision-trees-for-classification-id3-algorithm-explained-89df76e72df1)

> [!TLDR] 
> ID3 is a **top-down greedy** algorithm deals with **categorical predictors**, **categorical response** and **multi-way split**.

Let 
- response $y \in \{1, 2, \dots, K\}$;

## Grow Phase

1.  Calculate the Information Gain of each feature.
2.  Considering that all rows don’t belong to the same class, split the dataset **S** into subsets using the feature for which the Information Gain is maximum.
3.  Make a decision tree node using the feature with the maximum Information gain.
4.  If all rows belong to the same class, make the current node as a leaf node with the class as its label.
5.  Repeat for the remaining features until we run out of all features, or the decision tree has all leaf nodes.

>[!Algorithm]
> `ID3` (`Examples`, `Target_Attribute`, `Attributes`)
> 
>   1. Create a root node for the tree
>   2. Try split node:
>       - If all examples are positive, Return the single-node tree Root, with `label` = $+$.
>       - If all examples are negative, Return the single-node tree Root, with `label` = $-$.
>       - If number of predicting attributes is empty, then Return the single node tree Root, with `label` = most common value of the target attribute in the `Examples`.
>       - Otherwise Begin
>           1. $A \gets$ The Attribute that best classifies `Examples`.
>           2. Decision Tree attribute for `Root` = $A$.
>           3. For each possible value, $v_i$ of $A$,
>           4. Add a new tree branch below Root, corresponding to the test $A = v_i$.
>           5. Let `Examples`($v_i$) be the subset of `Examples` that have the value $v_i$ for $A$
>           6. If `Examples`($v_i$) is empty.  Then below this new branch add a leaf node with `label` = most common target value in the `Examples`. Else below this new branch add the subtree `ID3` (`Examples`($v_i$), `Target_Attribute`, `Attributes` – $\{A\}$)
>        - End
>   3. Return Root

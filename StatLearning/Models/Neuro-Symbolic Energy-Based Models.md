#model-checking 
#logic 
#machine-learning 

# Energy-Based Model (EBM)

EBS measures the _compatibility_ of a collection of observed variables $x \in X$ and target variables $y\in Y$ with a scalar valued _energy function_. 
$$
  E: X\times Y \to \mathbb R 
$$

# Neural Symbolic Energy-Based Model (NeSyEBM)

Let input variables $\boldsymbol x = (\boldsymbol x_{nn}, \boldsymbol x_{sy})$ where

1. neural: $\boldsymbol x_{nn} \in X_{nn}$;
2. symbolic: $\boldsymbol x_{sy} \in X_{sy}$.


The parameters of the energy function are also partitioned:

1. neural: $\boldsymbol w_{nn} \in W_{nn}$;
2. symbolic $\boldsymbol w_{sy}\in W_{sy}$

The _energy function_:
$$
  E: Y \times X_{nn} \times W_{nn} \times X_{sy} \times W_{sy} \to \mathbb R
$$

 ![[NeuPSL-pipeline.png]]

- $\boldsymbol g_{nn} = \boldsymbol g(\boldsymbol x_{nn}, \boldsymbol w_{nn})$ is the output of the neural part
- $\psi_j(\boldsymbol g_{nn}, \boldsymbol x, \boldsymbol y)\in \mathbb R$ is dubbed as _symbolic potential_.
- $E(\boldsymbol \psi, \boldsymbol w_{sy}) \ge 0$ is the _energy function_. 

The _symbolic potentials_ and _neural networks_ together define a _deep hinge-loss Markov random field (DeepHL-MRF)_, a tractable probabilistic graphical model that supports scalable _convex joint inference_.


 # Neural Probabilistic Soft Logic

> NeuPSL instantiates the _symbolic potentials_ of its energy function using the PSL language where dependencies between relations and attributes of entities in a domain, defined as **atoms**, are encoded with _weighted first-order logical clauses_ and linear arithmetic inequalities referred to as **rules**.


# Deep Hinge-loss Markov Random Field and Learning

1. _deep hinge loss potential_ $\boldsymbol \phi = (\phi_1, \dots, \phi_m)$:
$$ \phi_j(\boldsymbol y, \boldsymbol x_{sy}, \boldsymbol x_{nn}, \boldsymbol w_{nn}) = \max\big\{ \ell_j(\boldsymbol y, \boldsymbol x_{sy}, \boldsymbol g_{nn}(\boldsymbol x_{nn}, \boldsymbol w_{nn})) , 0 \big \}^\alpha $$
  where $\alpha \in \{1, 2\}$, and $\ell_j$ are affine functions
2. _energy_
$$
  E(\boldsymbol y, \boldsymbol x_{sy}, \boldsymbol x_{nn}, \boldsymbol w_{nn}, \boldsymbol w_{psl}) = \boldsymbol w_{psl}^T \boldsymbol \phi
$$
3. DHL-MRF
$$
  \Pr(\boldsymbol y| \boldsymbol x_{sy}, \boldsymbol x_{nn}) = 
    \frac{1}{Z}e^{-E(\boldsymbol y, \boldsymbol x_{sy}, \boldsymbol x_{nn}, \boldsymbol w_{nn}, \boldsymbol w_{psl})} \cdot \mathbb 1_{(\boldsymbol y, \boldsymbol x_{sy})\in \Omega}
$$
where $\Omega$ is defined by _hard constrains_.

Define the _loss function_ for learning the weights
$$
  L(\boldsymbol w_{nn}, \boldsymbol w_{psl}) = \sum_i E(\boldsymbol y^{(i)}, \boldsymbol x_{sy}^{(i)}, \boldsymbol x_{nn}^{(i)}, \boldsymbol w_{nn}, \boldsymbol w_{psl})
$$
And for logical inferencing/prediction
$$
  \hat {\boldsymbol y} = \arg\min_{\boldsymbol y}   E(\boldsymbol y, \boldsymbol x_{sy}, \boldsymbol x_{nn}, \boldsymbol w_{nn}, \boldsymbol w_{psl})
$$
>[!NOTE] constraint learning 
>  the energy may not be relevant to $\boldsymbol y$, if so it is a constraint learning problem.  

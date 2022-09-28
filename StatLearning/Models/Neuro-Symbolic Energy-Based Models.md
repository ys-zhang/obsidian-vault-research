#model-checking 
#logic 
#machine-learning 

# Energy-Based Model (EBM)

EBS measures the _compatibility_ of a collection of observed variables $x \in X$ and target variables $y\in Y$ with a scalar valued _energy function_. 
$$
  E: X\times Y \to \mathbb R 
$$

# Neuro-Symbolic Energy-Based Model (NeSyEBM)

Input variables are organised into 

1. neural: $x_{nn} \in X_{nn}$;
2. symbolic: $x_{sy} \in X_{sy}$.

The parameters of the energy function are also partitioned:

1. neural: $w_{nn} \in W_{nn}$;
2. symbolic $w_{sy}\in W_{sy}$

The _energy function_:
$$
  E: Y \times X_{nn} \times W_{nn} \times X_{sy} \times W_{sy} \to \mathbb R
$$

 ![[NeuPSL-pipeline.png]]

- $g_{nn} = g(x_{nn}, w_{nn})$ is the output of the neural part
- $\psi(g_{nn}, x, y)\in \mathbf R$ is dubbed as _symbolic potential_.
- $E(\psi, w_{sy})$ is the energy function. 

The _symbolic potentials_ and _neural networks_ together define a _deep hinge-loss Markov random field (DeepHL-MRF)_, a tractable probabilistic graphical model that supports scalable _convex joint inference_.

 # Neural Probabilistic Soft Logic

> NeuPSL instantiates the _symbolic potentials_ of its energy function using the PSL language where dependencies between relations and attributes of entities in a domain, defined as **atoms**, are encoded with _weighted first-order logical clauses_ and linear arithmetic inequalities referred to as **rules**.

 
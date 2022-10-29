#DSL 
#machine-learning 


# Theory

Probabilistic programs are “usual” programs with two add-on structs:

1. the ability to _draw values_ at random from distributions,
2. the ability to _condition values of variables_ in a program via _observe statements_


## Denotational Semantics 

- $\Sigma$ denotes the set of all program states.
- $\sigma\in\Sigma$ denotes a **state** of the program is a _partial valuation_ to all its variables.
    - type of $\sigma$: $$ \sigma : \mathbf{Var}  \to \mathbf{Val} $$
    - lifting $\sigma$ by assigning $\bot$ to uninitialised variables: $$ \sigma: \mathbf{Expr} \to \mathbf{Val} $$

- denotational semantics (expectation) $$ [[\mathcal S]](f)(\sigma) $$      
    - The meaning of a probabilistic program is the _expected value_ of its _return expression_. 
    - The **return expression** of a probabilistic program if a function $$ f: \Sigma \to \mathbb R_{\ge 0} $$
    - $\mathcal S$ is a statement where $$ [[\mathcal S]]: (\Sigma \to \mathbb R_{\ge 0}) \to \Sigma \to \mathbb R_{\ge 0} $$
    - $f$ is the return expression
    - $\sigma$ is the initial program state
    - The meaning of the **probabilistic assignment** is the expected value obtained by sampling $v$ from the distribution `Dist`, executing the assignment with $v$ as the RHS value, and applying $f$ on the resulting state (the expectation is the integral over all possible values $v$).
    ![[Screen Shot 2022-10-12 at 11.39.58 pm.png]]

## Expressiveness

- _Bayesian Networks(DAG)_ can be represented as a loop-free _probabilistic program_.
- [[DTMC (discrete-time Markov chain)]] can be represented as a loopy _probabilistic program_.
- _Probabilistic database_


# Pyro

## Modelling Bayesian Network

In the graphical models literature, it is common to distinguish between **inference** and **learning**.

- _learning_ means calculating MAP/MLE of parameters
- _inferencing_ means computing (functions of) distribution of posterior distribution of **latent** variables.

| Concept              | Code                             |
| -------------------- | -------------------------------- |
| observable variables | `visible_x = pyro.sample(...)`   |
| latent variables     | `invisible_x = pyro.sample(...)` |
| parameters           | `theta = pyro.param(...)`        |
| plates               | `pyro.plate()`                   | 


![[plate-notation.excalidraw|100%]]


## Inferencing in Pyro

Besides exact inferencing methods, [[variational inference]] is the most used approximate inference algorithm.

Pyro uses a **guide function** to specify [[variational inference#Variational Distribution Family|variational distribution]].

- the guide needs to provide a valid joint probability density over _all the latent random variables_ in the model.
- the variable are matched through _matching variable names_, which is specified in `pyro.sample("name", ...)` 
- use `pyro.infer.autoguide`


```python
pyro.clear_param_store()

# These should be reset each training loop.
auto_guide = pyro.infer.autoguide.AutoNormal(model)
adam = pyro.optim.Adam({"lr": 0.02})  # Consider decreasing learning rate.
elbo = pyro.infer.Trace_ELBO()
svi = pyro.infer.SVI(model, auto_guide, adam, elbo)

losses = []
for step in range(1000 if not smoke_test else 2):  # Consider running for more steps.
    loss = svi.step(is_cont_africa, ruggedness, log_gdp)
    losses.append(loss)
    if step % 100 == 0:
        logging.info("Elbo loss: {}".format(loss))

plt.figure(figsize=(5, 2))
plt.plot(losses)
plt.xlabel("SVI step")
plt.ylabel("ELBO loss");
```


### Subsampling (Mini-batch)

For theory see [[variational inference#Mini-batch Dealing with Large Sample Size Version 2|mini-batching in VI]]

In Pyro, using `plate` or `markov` to indicate _local latent random variables_
- use the argument `subsample_size` in `plate()` to specify the _mini-batch size_. 
- by default pyro's subsample strategy is sampling with replace, which can be changed using the `subsample` argument in the `plate()` function.  


### Dependency Tracking

Tracking dependency within a stochastic function that includes arbitrary Python code is a bit tricky. The approach currently implemented in Pyro is analogous to the one used in **WebPPL**. 

Briefly, a conservative notion of dependency is used that relies on _sequential ordering_. If random variable $z_2$ follows $z_1$ in a given stochastic function then $z_2$ _may be_ dependent on $z_1$ and therefore _is_ assumed to be dependent. 

To mitigate the overly coarse conclusions that can be drawn by this kind of dependency tracking, Pyro includes constructs for declaring things as _independent_, namely `plate` and `markov`

see also:

1. [SVI](https://pyro.ai/examples/svi_part_ii.html)
2. [VAE (Variational AutoEncoder)](https://pyro.ai/examples/vae.html)




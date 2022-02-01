# Concepts

### Function

![[tf function.excalidraw]]

### Graph

_Graphs_([`tf.Graph`](https://www.tensorflow.org/api_docs/python/tf/Graph)) are used by `tf.function`s to represent the function's computations. 
1. Each _graph_ contains a set of `tf.Operation` objects, which represent _units of computation_; 
2. and `tf.Tensor` objects, which represent the _units of data_ that flow between operations.

A default graph can be registered with the `tf.Graph.as_default`context manager. Then, operations will be added to the graph instead of being executed eagerly.


### Collection

A _Graph_ instance supports an arbitrary number of "_collections_" that are identified by name. 

For convenience when building a large graph, _collections_ can store groups of related objects: for example, the `tf.Variable`uses a collection (named `tf.GraphKeys.GLOBAL_VARIABLES`) for all variables that are created during the construction of a graph.


### Variable

A [`tf.Variable`](https://www.tensorflow.org/api_docs/python/tf/Variable) maintains shared and persistent state manipulated by a program.  

1. Its value can be changed using one of the assign methods.
2. Often used to hold _trainable model parameters_.


### Gradient tape


## Saved Model

A _Saved Model_ is a directory containing serialized _signatures_ and the state needed to run them, including _variable values_ and _vocabularies_.

_Saved Models_ may contain multiple variants of the model (multiple `v1.MetaGraphDefs`, identified with the `--tag_set` flag to `saved_model_cli`), but this is rare.

## References

1. [Introduction to graphs and `tf.function`](https://www.tensorflow.org/guide/intro_to_graphs)
2. [A tour of saved model signatures](https://blog.tensorflow.org/2021/03/a-tour-of-savedmodel-signatures.html)
3. [Using the saved model format](https://www.tensorflow.org/guide/saved_model)
4. [Better performance with `tf.function`](https://www.tensorflow.org/guide/function)



# API

There are three ways of creating a model in `tf.keras`: 
- **Sequential API** 
- **Functional API**
- **Model sub-classing**.

##  Tensorflow 1.0

```python
import tensorflow.compat.v1 as tf 
tf.disable_v2_behavior() 
in_a = tf.placeholder(dtype=tf.float32, shape=(2)) 

def model(x): 
	with tf.variable_scope("matmul"): 
		W = tf.get_variable("W", initializer=tf.ones(shape=(2,2))) 
		b = tf.get_variable("b", initializer=tf.zeros(shape=(2))) 
		return x * W + b out_a = model(in_a) 
	
with tf.Session() as sess: 
	sess.run(tf.global_variables_initializer()) 
	outs = sess.run([out_a], feed_dict={in_a: [1, 0]}) 
	writer = tf.summary.FileWriter("./logs/example", sess.graph)
```

tensor board:
```bash
tensorboard --logdir=./logs/example/
```

## Functional API

```python

import tensorflow as tf 

W = tf.Variable(tf.ones(shape=(2,2)), name="W") 
b = tf.Variable(tf.zeros(shape=(2)), name="b") 

@tf.function 
def model(x): 
	return W * x + b out_a = model([1,0]) print(out_a)
out_a = model([1,0]) 
print(out_a)
```

# NN Concepts

## Activation function

![[Pasted image 20210825004552.png]]
### Perceptron

$$
	f(x) = \mathbb 1_{\langle w, x \rangle + b > 0}
$$

![[Pasted image 20210825002025.png]]

### Sigmoid

$$
\sigma(z) = \frac{1}{1 + \exp(-z)}
$$

![[Pasted image 20210825003014.png]]

### tanh


$$
\tanh(z) = \frac{e^z - e^{-z}}{e^z + e^{-z}}
$$

![[Pasted image 20210825003225.png]]

### ReLU

$$
f(z) = \mathbb 1(z \ge 0) \cdot z
$$

ReLU is not a [[saturating activation function]]
![[Pasted image 20210825003434.png]]

### ELU 

$$
f(z;a) = \begin{cases}
	a(e^z - 1) &\quad z \le 0 \\
	x          &\quad z > 0 
\end{cases}
$$
where $a>0$.

![[Pasted image 20210825004425.png]]


### Leaky ReLU

$$
f(z;a) = \begin{cases}
	az &\quad z \le 0 \\
	x          &\quad z > 0 
\end{cases}
$$
where $a>0$.

![[Pasted image 20210825004532.png]]

## Loss function

see [3.3. Metrics and scoring: quantifying the quality of predictions — scikit-learn 0.24.2 documentation](https://scikit-learn.org/stable/modules/model_evaluation.html) or [[model evaluation]]

### MSE
### binary cross-entropy

$$
\mathscr l(x,y) =L=\{l_1,\dots,l_N\}^⊤ 
$$
$$
l_n =−w_n [y_n\cdot \log x_n +(1−y_n)\cdot \log(1−x_n)]
$$


### categorical cross-entropy

## Metrics

Metrics are similar to objective functions, with the only difference that they are not used for training a model, but only for evaluating the model.

|     | Positive Prediction | Negative Prediction |
| --- | ------------------- | ------------------- |
|Positive Class | True Positive (TP)  | False Negative (FN)
|Negative Class | False Positive (FP) | True Negative (TN)

- Accuracy:  proportion of correct predictions with respect to the targets
- Precision： $$TP \over (TP + FP)$$，$P(T|calc=T)$
- Recall: $$TP \over (TP + FN)$$, $P(calc=T|T)$


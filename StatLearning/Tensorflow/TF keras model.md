# The big picture

![[Pasted image 20210901132550.png]]
we have 3 low-level concepts defined `keras/engine/`:

> 1. `KerasTensor`s are tensor-like objects that represent the symbolic inputs and outputs of Keras layers during Functional model construction. They are comprised of the `tf.TypeSpec` of the (Composite)Tensor that will be consumed/produced in the corresponding location of the Functional model.

> 2.  A `Node` describes the connectivity between two layers. Each time a layer is connected to some new input, a node is added to `layer._inbound_nodes`. Each time the output of a layer is used by another layer, a node is added to `layer._outbound_nodes`.

> 3. A `Layer` is a callable object that takes as input one or more tensors and that outputs one or more tensors. It involves *computation*, defined in the `call()` method, and a *state* (weight variables), defined either in the constructor `__init__()` or in the `build()` method.

```python
# 建立新的keras Tensor
a = Input(shape=(32,), name='input_a')
b = Input(shape=(32,), name='input_b')

a_layer, a_node_index, a_tensor_index = a._keras_history
assert len(a_layer.inbound_nodes) == 1
assert a_tensor_index is 0

# node和layer之间的关系
node = a_layer.inbound_nodes[a_node_index]
assert node.outbound_layer == a_layer

# 建立连接层，将Tensor传入
dense = Dense(16, name='dense_1')
a_2 = dense(a)
b_2 = dense(b)

assert len(dense.inbound_nodes) == 2
assert len(dense.outbound_nodes) == 0

# 与张量a关联的Node
assert dense.inbound_nodes[0].inbound_layers == [a_layer]
assert dense.inbound_nodes[0].outbound_layer == dense
assert dense.inbound_nodes[0].input_tensors == [a]

# 与张量b关联的Node
assert dense.inbound_nodes[1].inbound_layers == [b_layer]
assert dense.inbound_nodes[1].outbound_layer == dense
assert dense.inbound_nodes[1].input_tensors == [b]
```


# Input dimension of layers 

The dimension / shape:

	(batch_size, num_of_feature_in_axis_1, num_of_feature_in_axis_2, ...)
	
Generally the batch size is inferred on DAG build, i.e. in `Layer.call`. Thus
```python
import tensorflow as tf

inputs = tf.keras.Input((3, 2, 4)) 
inputs.shape    # gives TensorShape([None, 3, 2, 4])
```


# fit and the train loop

```python

# 定义损失函数
loss_fn = keras.losses.SparseCategoricalCrossentropy(from_logits=True)

epochs = 2
for epoch in range(epochs):
    print("\nStart of epoch %d" % (epoch,))

    # Iterate over the batches of the dataset.
    for step, (x_batch_train, y_batch_train) in enumerate(train_dataset):

        # Open a GradientTape to record the operations run
        # during the forward pass, which enables auto-differentiation.
        with tf.GradientTape() as tape:

            # Run the forward pass of the layer.
            # The operations that the layer applies
            # to its inputs are going to be recorded
            # on the GradientTape.
			# 这里输出的是预测即最后output layer 的结果
            logits = model(x_batch_train, training=True)  # Logits for this minibatch

            # Compute the loss value for this minibatch.
			# 计算 loss
            loss_value = loss_fn(y_batch_train, logits)

        # Use the gradient tape to automatically retrieve
        # the gradients of the trainable variables with respect to the loss.
		# 计算 d[loss]/d[weights]
        grads = tape.gradient(loss_value, model.trainable_weights)

        # Run one step of gradient descent by updating
        # the value of the variables to minimize the loss.
        optimizer.apply_gradients(zip(grads, model.trainable_weights))

        # Log every 200 batches.
        if step % 200 == 0:
            print(
                "Training loss (for one batch) at step %d: %.4f"
                % (step, float(loss_value))
            )
            print("Seen so far: %s samples" % ((step + 1) * batch_size))

```


## Loss function

as stated in the documentation from `Model.compile`

loss: String (name of objective function), objective function or `tf.keras.losses.Loss` instance. See `tf.keras.losses`. 

An objective function is any callable with the signature 

	loss = fn(y_true, y_pred)
where 

- **y_true = ground truth values** with shape = `[batch_size, d0, .. dN]`, except sparse loss functions such as *sparse categorical crossentropy* where shape = `[batch_size, d0, .. dN-1]`.

- **y_pred = predicted values** with shape = `[batch_size, d0, .. dN]`. It returns a weighted loss float tensor. 
	> If a custom `Loss` instance is used and reduction is set to `None`, return value has the shape `[batch_size, d0, .. dN-1]` i.e. **per-sample or per-time step loss values; otherwise, it is a scalar**. 
	
> If the model has multiple outputs, you can use a different loss on each output by passing a dictionary or a list of losses. **The loss value that will be minimized by the model will  then be the sum of all individual losses**.


adding loss functions:

```python
model.compile(loss=loss_fn)          # option one
model.add_loss(some_tensor_as_loss)  # option two  
```


### Reduction of loss to a scalar

[tensorflow - Where exactly are the KL losses used after the forward pass? - Stack Overflow](https://stackoverflow.com/questions/61314548/where-exactly-are-the-kl-losses-used-after-the-forward-pass)

in brief:
> Note that loss function by definition **must** return **only a single value for each input sample** (not the whole batch). Therefore, the `total_loss` would be a 1D tensor and `K.mean` gives you the average of all of them. I am %99 sure because that's what it should be. But maybe that %1 uncertainty proves me wrong. It could be easily verified by setting a trace at that line in debugging mode


from `keras/losses.py`

```python
@keras_export('keras.losses.Loss')
class Loss:
	def __init__(self, reduction=losses_utils.ReductionV2.AUTO, name=None):
		losses_utils.ReductionV2.validate(reduction)
		self.reduction = reduction  # here defines the reduction field
```

Lets inspect the default settings of the `reduction` function. From `keras.utils.losses_utils.py` and class `keras.losses.Reduction`:

> `AUTO`: Indicates that the reduction option will be determined by the usage
     context. For almost all cases this defaults to `SUM_OVER_BATCH_SIZE`. When
     used with `tf.distribute.Strategy`, outside of built-in training loops such
     as `tf.keras` `compile` and `fit`, we expect reduction value to be
     `SUM` or `NONE`. Using `AUTO` in that case will raise an error.

**So how this `reduction` function be called?**

```python
	class Model(Layer)
		def compile(sefl, loss, ...):
			...
			self.compiled_loss = compile_utils.LossesContainer(
					loss, loss_weights, output_names=self.output_names)
			self.loss = loss or {} # Backwards compat.
			...
			
		def fit(self, ...):
			...
			self.train_step(...)
		
		def train_step(self, ...):
			# the compiled_loss field is an instance of 
			#    `LossesContainer`
			# N.B. type of loss:
			#      (total_loss, per_output_loss_list)
			loss = self.compiled_loss(
				y, y_pred, sample_weight,
				regularization_losses=self.losses) 
			self.optimizer.minimize(loss, self.trainable_variables, 
									tape=tape)
			self.compiled_metrics.update_state(y, y_pred, sample_weight)
			...
			
		# inherited from class Layer
		@property
		def losses(self):
			""" List of losses added using the `add_loss()` API. """
			# this function returns losses defines the regulations
			#  i.e. the panalties.
			...
```

1. provide loss to `Model` in `Model.compile`:
2. loss is calculated in `Model.train_step`
3. loss is calculated at `LossesContainer.__call__`

```python

# defined in losses_utils
def scale_loss_for_distribution(loss_value):
	"""Scales and returns the given loss value by the number of replicas."""
	num_replicas = (distribution_strategy_context.  \
	     			get_strategy().                 \
				    num_replicas_in_sync)
	if num_replicas > 1:
 		loss_value *= (1. / num_replicas)
	return loss_value


# HERE IS WHERE COMPUTATION HAPPENS
def compute_weighted_loss(losses,
                          sample_weight=None,
                          reduction=ReductionV2.SUM_OVER_BATCH_SIZE,
                          name=None):
	"""
	Returns:
    Weighted loss `Tensor` of the same type as `losses`. If `reduction` is
    `NONE`, this has the same shape as `losses`; otherwise, it is scalar.
	"""
	...
	loss = reduce_weighted_loss(weighted_losses, reduction)
	...
	return loss

def reduce_weighted_loss(weighted_losses,
                         reduction=ReductionV2.SUM_OVER_BATCH_SIZE):
  """Reduces the individual weighted loss measurements."""
	if reduction == ReductionV2.NONE:
		loss = weighted_losses
	else:
		loss = math_ops.reduce_sum(weighted_losses)
    	if reduction == ReductionV2.SUM_OVER_BATCH_SIZE:
      		loss = _safe_mean(loss, _num_elements(weighted_losses))
	return loss


class LossesContainer(Container)：
	def __call__(self,
               y_true,
               y_pred,
               sample_weight=None,
               regularization_losses=None):
		for ... :
			# for each loss defined
			loss_value = loss_obj(y_t, y_p, sample_weight=sw)
			...
			if (loss_obj.reduction == 
				losses_utils.ReductionV2.SUM_OVER_BATCH_SIZE or
				loss_obj.reduction == losses_utils.ReductionV2.AUTO):
        		loss_value = losses_utils.\
					scale_loss_for_distribution(loss_value)
		# Adds all input tensors element-wise.
		total_loss = math_ops.add_n(loss_values)  
        return total_loss

class Loss:
	def __call__(self, y_t, y_p, weight):
		losses = call_fn(y_true, y_pred)
    	return losses_utils.compute_weighted_loss(
        	losses, sample_weight, 
			reduction=self._get_reduction())
	def _get_reduction(self):
		"""Handles `AUTO` reduction cases and returns the reduction 
		   value.
		"""
		if (not self._allow_sum_over_batch_size and
			distribution_strategy_context.has_strategy() and
			(self.reduction == losses_utils.ReductionV2.AUTO or
			 self.reduction == 
			 losses_utils.ReductionV2.SUM_OVER_BATCH_SIZE)):
		 	raise ValueError(
			  'Please use `tf.keras.losses.Reduction.SUM` or '
			  '`tf.keras.losses.Reduction.NONE` for loss reduction when losses are '
			  'used with `tf.distribute.Strategy` outside of the built-in training '
			  'loops. You can implement '
			  '`tf.keras.losses.Reduction.SUM_OVER_BATCH_SIZE` using global batch '
			  'size like:\n```\nwith strategy.scope():\n'
			  '    loss_obj = tf.keras.losses.CategoricalCrossentropy('
			  'reduction=tf.keras.losses.Reduction.NONE)\n....\n'
			  '    loss = tf.reduce_sum(loss_obj(labels, predictions)) * '
			  '(1. / global_batch_size)\n```\nPlease see '
			  'https://www.tensorflow.org/tutorials/distribute/custom_training'
			  ' for more details.')

		if self.reduction == losses_utils.ReductionV2.AUTO:
			return losses_utils.ReductionV2.SUM_OVER_BATCH_SIZE
		return self.reduction
		

```

how user defined loss functions acts?

1. passed to `Model.compile` in which
2. passed to `LossesContainer.__init__` in which
3. passed to `LossesContainer._get_loss_object` in which wrapped in
4. `LossFunctionWrapper` and set `_allow_sum_over_batch_size=True`

### Losses in functional API
[使用内置方法进行训练和评估  |  TensorFlow Core](https://www.tensorflow.org/guide/keras/train_and_evaluate#%E8%87%AA%E5%AE%9A%E4%B9%89%E6%8D%9F%E5%A4%B1)





## Metrics

How metrics are aggregated in a epoch?

```python

class Model:
	
	@property
	def metrics(self):
		"""
		Returns the model's metrics added using `compile`, 
			`add_metric` APIs
		"""
		...
	
	def train_step(self, data):
		...
		self.compiled_metrics.update_state(y, y_pred, sample_weight)
		# Collect metrics to return
		return_metrics = {}
		for metric in self.metrics:
			result = metric.result()
			if isinstance(result, dict):
				return_metrics.update(result)
			else:
				return_metrics[metric.name] = result
		return return_metrics

```

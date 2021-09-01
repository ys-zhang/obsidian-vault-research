# Position of TF 1.0 API

- low level: Tensor, Tensor Ops, back-prop;
- high level: layer, loss function, optimizer, metrics and training loop


1. tensors are immutable, use `tf.Variable` if you want to change the value.
	- `tf.Variable(initial_value=...)`
2. tensor attributes:
	1. `ts.ndim`: rank of the tensor
	2. `ts.shape`
	3. `ts.dtype`
# Tensor 

## Attributes
- `tensor.shape`
- `tensor.dtype`
- `tensor.device`: CPU or GPU
```python
# We move our tensor to the GPU if available
if torch.cuda.is_available():
    tensor = tensor.to('cuda')
```
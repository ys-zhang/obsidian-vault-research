#todo 
# Question
In 2006 by new initialization or training mechanisms, DNN are first successfully trained.

> why standard gradient descent from random initialization is doing so poorly with deep neural networks?


# Observations

1. The **logistic sigmoid activation** is unsuited for deep networks with **random initialization** because of its mean value, which can drive especially the top hidden layer into saturation.
2. Saturated units can move out of saturation by themselves, albeit slowly.
3. A new non-linearity that saturates less can often be beneficial.

A Bloom filter is a **space-efficient probabilistic** data structure that is used to test whether an element is a member of a set.

![[Pasted image 20210830144108.png]]

The price we pay for efficiency is that it is probabilistic in nature that means, there might be some **False Positive** results. 

	Avoid False Negative, control False Positive
	
# Probability Model

suppose bitmap length is $m$, $n$ is the expect number of inputs inserted, number of hash function is  $k$

###### 1. FP (False Positive)

$$
(1 - (1-\frac{1}{m})^{kn})^k
$$
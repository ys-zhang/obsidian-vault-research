

The number of bit positions in which two codewords differ is called the **Hamming distance**.

Its significance is that if two codewords are a Hamming distance $d$ apart, it will require $d$ single-bit errors to convert one into the other.

legal codewords density, consider $n=m+r$ with $m$ data bits and $r$ check bits.
$$
	\frac{2^m}{2^n} = \frac{1}{w^2}
$$

> It is the sparseness with which the message is embedded in the space of codewords that allows the receiver to detect and correct errors


The basic idea of correction is correct to the nearest legal codeword.
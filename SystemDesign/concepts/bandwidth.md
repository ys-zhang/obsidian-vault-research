# Theoretical upper bound


We call the rate at which the signal changes the symbol rate to distinguish it from the **bit rate**.

The bit rate is the **symbol rate** multiplied by the **number of bits per symbol**.

$$
	bitrate = symbolrate \times bits/symbol
$$

**baud rate** is the same as **symbol rate**.

## Nyquist's Theorem

Consider a perfect network with bandwidth $B$ (in unit $\mathrm Hz$). If the signal consists of $V$ discrete levels, then
$$
	\max \mathrm{data} \; \mathrm{rate} = 2B \log_2 V \quad \mathrm{bits/sec}
$$

## Shannon's Theorem
Shannon’s major result is that the maximum data rate or capacity of a noisy channel whose bandwidth is $B \mathrm Hz$ and whose signal-to-noise ratio is $S/N$, is given by:
$$
	B \log_2 (1 + S/N)
$$






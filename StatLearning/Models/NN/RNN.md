---

excalidraw-plugin: parsed

---
==⚠  Switch to EXCALIDRAW VIEW in the MORE OPTIONS menu of this document. ⚠==



# Notes

[Keras RNN Guide](https://keras.io/guides/working_with_rnns/)

## The Problem


giving a sequence $x^{(i)}= ( x^{(i)\langle1\rangle}, x^{(i)\langle2\rangle}, \cdots, x^{(i)\langle n\rangle}, \cdots )$,or $x^{(i)}=<x^{(i)}_1, x^{(i)}_2, \cdots, x^{(i)}_n, \cdots>$
predict the density
$$
	p(x_t|x_{1:t-1})
$$

The problem of [[CNN]] in this case is that the dimension of the input/sample is not fixed.

## Notation
input $x \in \cup_d \mathbb R^d$, output $y$

$t$th dimension of $i$th sample 
$$
	x^{(i)\langle t \rangle}
$$

## RNN

$$
\begin{align}
	y^{\langle t \rangle} &= g(h^{\langle t \rangle})\\
	h^{\langle t \rangle} &= f(h^{\langle t-1 \rangle}, x^{\langle t \rangle}) 
\end{align}
$$

```python
tf.keras.layers.RNN(
    cell,
    return_sequences=False,  # whether returns sequence output: $y(t)$
    return_state=False,      # whether returns hidden state: $h(T)$
    go_backwards=False,
    stateful=False,
    unroll=False,
    time_major=False,
    **kwargs
)
```

## Simple RNN

in simple RNN $y = g(h) = h$.

$$
\begin{align}
	y^{\langle t \rangle} &= h^{\langle t \rangle}   \\
	h^{\langle t \rangle} &= f(a^{\langle t \rangle}) \\
	a^{\langle t \rangle} &= U*x^{\langle t \rangle} + V*h^{\langle t-1 \rangle} + b
\end{align}
$$


in `keras` we have the `layers.SimpleRNN` layer

```python

# simple RNN
tf.keras.layers.SimpleRNN(
    units,								  # dim of output/hidden-state
    activation="tanh",      			  # `f`
    use_bias=True,          			  # `b != 0`
    kernel_initializer="glorot_uniform",  # `U`
    recurrent_initializer="orthogonal",   # `R`
    bias_initializer="zeros",             # `b`
    kernel_regularizer=None,
    recurrent_regularizer=None,
    bias_regularizer=None,
    activity_regularizer=None,
    kernel_constraint=None,
    recurrent_constraint=None,
    bias_constraint=None,
    dropout=0.0,
    recurrent_dropout=0.0,
    return_sequences=False,               # 
    return_state=False,                   # 
    go_backwards=False,
    stateful=False,
    unroll=False,
    **kwargs
)


```

## GRU


![[Pasted image 20210906191108.png]]
Defining the **reset gate** $r$, and the **update gate** $z$

$$
\begin{align}
	r^{\langle t \rangle} &= \sigma(W_r * x^{\langle t \rangle} + U_r*h^{\langle t-1 \rangle} + b_r)\\
	
	z^{\langle t \rangle} &= \sigma(W_z * x^{\langle t \rangle} + U_z*h^{\langle t-1 \rangle} + b_z)
	
\end{align}
$$

where $\sigma$ is the logistic sigmoid.

and hidden state update as 

$$
\begin{align}
	h^{\langle t \rangle} &= z^{\langle t \rangle} * h^{\langle t-1 \rangle}  + (1-z^{\langle t \rangle}) * \tilde{h}^{\langle t \rangle} \\
	
	\tilde{h}^{\langle t \rangle} &= \phi(W*x^{\langle t \rangle} + U* (r \odot h^{\langle t-1 \rangle}))
\end{align}
$$

> when the reset gate is close to $0$, the hidden state is forced to ignore the previous hidden state and reset with the current input only.
> the update gate controls how much information from the previous hidden state will carry over to the current hidden state.     


```python
tf.keras.layers.GRU(
    units,
    activation="tanh",                       # phi
    recurrent_activation="sigmoid",          # sigma
    use_bias=True,
    kernel_initializer="glorot_uniform",     # W
    recurrent_initializer="orthogonal",      # U
    bias_initializer="zeros",
    kernel_regularizer=None,
    recurrent_regularizer=None,
    bias_regularizer=None,
    activity_regularizer=None,
    kernel_constraint=None,
    recurrent_constraint=None,
    bias_constraint=None,
    dropout=0.0,
    recurrent_dropout=0.0,
    return_sequences=False,
    return_state=False,
    go_backwards=False,
    stateful=False,
    unroll=False,
    time_major=False,
    reset_after=True,
    **kwargs
)
```


# Text Elements
x ^gOx9BQNB

y ^6KSXp7Ft

hidden layer ^RpCf6rN5

input sequence ^pGGYuNZa

response sequence ^qNFsKwcS

activation ^en0jDMDO

VANILLA RNN ^lrjg188r

Some Problem OF vanilla RNN ^51q7S0hL

1. only take information from the past not the future see [[BRNN]] ^kNPMYtPp

Many-To-One Architecture ^J7dLydJz

Many-to-Many Architecture ^GW6WfaUZ

One-To-Many Architecture ^ulNP2QZB

ENCODER ^8l9xUD43

DECODER ^WdDzHPx5

Seq2Seq ^3wBFZu4k

LSTM (long short term memory) ^FuZQcFqs

hidden state ^GymB8DNZ

h(t) := f(h(t-1), x(t))
y(t) := g(h(t)) ^7zYkzFIs

y(t) := h(t)
h(t) := f( a(t) )
a(t) := U * x(t) + V * h(t-1) + b ^6KEW8EUS

activation function ^Cgb8RTJT

kernel-weight ^gZfdMFC6

dimension: dx * dh ^T0CMwoIF

recurrent-weight ^UpbHhubK

dimension: dh * dh ^DiROSqZt

bias ^BddSf0L1

dimension: dh ^CdiAC1y5

Simple RNN ^wO9WNHfo

encoder:
    h<t> := f(h<t-1>, x<t>)
    c    := h<T>
decoder:
    h<t> := f'(h<t-1>, y<t>, c)
    P(y<t>|y<1:t-1>) := g(h<t>, y<t-1>, c)  ^2XQBXydL

GRU (gated recurrent unit) ^zBLkIRQf

x ^PNi1NO8v

y ^BcUu9HIF

%%
# Drawing
```json
{
	"type": "excalidraw",
	"version": 2,
	"source": "https://excalidraw.com",
	"elements": [
		{
			"type": "ellipse",
			"version": 79,
			"versionNonce": 1027508121,
			"isDeleted": false,
			"id": "zKKAJKCNqRrQz-379m5LH",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -860,
			"y": -180,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 27749945,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Ap_5sLZRbEXuwrjVIop6S"
			]
		},
		{
			"type": "ellipse",
			"version": 78,
			"versionNonce": 562174583,
			"isDeleted": false,
			"id": "XHPtXnErXKxY_MOGe0WTB",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -860,
			"y": 60,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 1635282391,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Jni41omPrxBeE9XGXxow7"
			]
		},
		{
			"type": "text",
			"version": 23,
			"versionNonce": 1125919865,
			"isDeleted": false,
			"id": "gOx9BQNB",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -800,
			"y": -180,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 16,
			"height": 35,
			"seed": 1979357943,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "x",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "x"
		},
		{
			"type": "text",
			"version": 27,
			"versionNonce": 761639831,
			"isDeleted": false,
			"id": "6KSXp7Ft",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -800,
			"y": 60,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 13,
			"height": 35,
			"seed": 1892283385,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "y",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "y"
		},
		{
			"type": "ellipse",
			"version": 126,
			"versionNonce": 196228441,
			"isDeleted": false,
			"id": "aUc_Gw_lCzeTIjwWmYA-W",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -280,
			"y": -190.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 1341793303,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"0kuFnGa1pbKqvThRl-8Gi",
				"Q1fJNkOij8UEUTOYbUETz"
			]
		},
		{
			"type": "ellipse",
			"version": 125,
			"versionNonce": 1740187831,
			"isDeleted": false,
			"id": "y11jrCpnEO-omfHGHGxWf",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -180,
			"y": -190.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 258705719,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"rPd8raLHAArIzfQYC5w0d",
				"OvBXFsnuGdY5y3Qx-w0Wk",
				"Bm4RFb3NYodnarIyv4k3V"
			]
		},
		{
			"type": "ellipse",
			"version": 146,
			"versionNonce": 1783898681,
			"isDeleted": false,
			"id": "9ej88X4_bGMXHXVBjgZX1",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -80,
			"y": -190.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 366348729,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"M2WCS9EdUN9DmBkQTrnl7",
				"rPd8raLHAArIzfQYC5w0d",
				"-NzmDqBJ-pk_z5Iacro_7",
				"Nq9w3IvVQb_WWrTBkIgu_"
			]
		},
		{
			"type": "arrow",
			"version": 245,
			"versionNonce": 773913047,
			"isDeleted": false,
			"id": "Q1fJNkOij8UEUTOYbUETz",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -259.2806863207861,
			"y": -149.34563594124018,
			"strokeColor": "#e67700",
			"backgroundColor": "#4c6ef5",
			"width": 0.6854246593804874,
			"height": 78.01235029739729,
			"seed": 262647673,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "aUc_Gw_lCzeTIjwWmYA-W",
				"focus": -0.04518393614519731,
				"gap": 1
			},
			"endBinding": {
				"elementId": "XAfkJc4lvHn6_VWKOwVyM",
				"focus": -0.007530656024205806,
				"gap": 1.000000000000039
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.6854246593804874,
					78.01235029739729
				]
			]
		},
		{
			"type": "ellipse",
			"version": 131,
			"versionNonce": 2078634777,
			"isDeleted": false,
			"id": "XAfkJc4lvHn6_VWKOwVyM",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -280,
			"y": -70.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1669849239,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"ddsDWHT4fQ7WxlbOaT1Vz",
				"Bm4RFb3NYodnarIyv4k3V",
				"7mmieKGz7lH4cPAsSyeQF"
			]
		},
		{
			"type": "ellipse",
			"version": 132,
			"versionNonce": 900835063,
			"isDeleted": false,
			"id": "x0Tg5ziLGlxFkIhexUO7t",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -180,
			"y": -70.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1641371737,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"OvBXFsnuGdY5y3Qx-w0Wk",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"Bm4RFb3NYodnarIyv4k3V",
				"EmhoZ_XTk95ed8A9kP0u-"
			]
		},
		{
			"type": "ellipse",
			"version": 131,
			"versionNonce": 1960421369,
			"isDeleted": false,
			"id": "n4W9QAKIf8T66ngtaxNLO",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -80,
			"y": -70.33331298828125,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1393407415,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"-NzmDqBJ-pk_z5Iacro_7",
				"IkNd9d72O5JgKBplCKczo",
				"EmhoZ_XTk95ed8A9kP0u-",
				"S2gzCHtzXYCAwzMPb8zoo",
				"YO5hxAKxxJRWBVrEFuZdm"
			]
		},
		{
			"type": "arrow",
			"version": 201,
			"versionNonce": 485470231,
			"isDeleted": false,
			"id": "OvBXFsnuGdY5y3Qx-w0Wk",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160,
			"y": -150.33331298828125,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1981474105,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "y11jrCpnEO-omfHGHGxWf",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "x0Tg5ziLGlxFkIhexUO7t",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 201,
			"versionNonce": 791953625,
			"isDeleted": false,
			"id": "-NzmDqBJ-pk_z5Iacro_7",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -60,
			"y": -150.33331298828125,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 962324183,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "9ej88X4_bGMXHXVBjgZX1",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "n4W9QAKIf8T66ngtaxNLO",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 360,
			"versionNonce": 1037014327,
			"isDeleted": false,
			"id": "Bm4RFb3NYodnarIyv4k3V",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -240,
			"y": -50.33331298828125,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 60,
			"height": 0,
			"seed": 1426519031,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XAfkJc4lvHn6_VWKOwVyM",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "x0Tg5ziLGlxFkIhexUO7t",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 201,
			"versionNonce": 2022227385,
			"isDeleted": false,
			"id": "ddsDWHT4fQ7WxlbOaT1Vz",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -260,
			"y": -30.33331298828125,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 504585495,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XAfkJc4lvHn6_VWKOwVyM",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "-pYPtEGjk80FUH3TC2RfM",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 113,
			"versionNonce": 1068881495,
			"isDeleted": false,
			"id": "-pYPtEGjk80FUH3TC2RfM",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -280,
			"y": 49.66668701171875,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 86893529,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"ddsDWHT4fQ7WxlbOaT1Vz"
			]
		},
		{
			"type": "ellipse",
			"version": 109,
			"versionNonce": 536518297,
			"isDeleted": false,
			"id": "BvwF3-THxnrNKtZlZeVWK",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -180,
			"y": 49.66668701171875,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 401973815,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"1e6iGAnEz-qqChqmzbzYy",
				"Bm4RFb3NYodnarIyv4k3V"
			]
		},
		{
			"type": "arrow",
			"version": 134,
			"versionNonce": 1769403255,
			"isDeleted": false,
			"id": "1e6iGAnEz-qqChqmzbzYy",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160,
			"y": -30.33331298828125,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1163264185,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": {
				"elementId": "BvwF3-THxnrNKtZlZeVWK",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 134,
			"versionNonce": 1144873849,
			"isDeleted": false,
			"id": "wUaBrW66ZXV8jO822IzZK",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -60,
			"y": -30.33331298828125,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1670529879,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": {
				"elementId": "GjGV-yHyOmNiEDcs2RY_Q",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 115,
			"versionNonce": 861367447,
			"isDeleted": false,
			"id": "GjGV-yHyOmNiEDcs2RY_Q",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -80,
			"y": 49.66668701171875,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 136806809,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"1e6iGAnEz-qqChqmzbzYy",
				"wUaBrW66ZXV8jO822IzZK",
				"dZwulEwk8Tnh3m-G4SHON"
			]
		},
		{
			"type": "arrow",
			"version": 259,
			"versionNonce": 1637125209,
			"isDeleted": false,
			"id": "YO5hxAKxxJRWBVrEFuZdm",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -170.33331298828125,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 199.37970753351226,
			"height": 101.3199966914393,
			"seed": 2060838521,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "RpCf6rN5",
				"focus": 0.6575821723391487,
				"gap": 1
			},
			"endBinding": {
				"elementId": "n4W9QAKIf8T66ngtaxNLO",
				"focus": -0.3655683196210873,
				"gap": 7.823353230898135
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-199.37970753351226,
					101.3199966914393
				]
			]
		},
		{
			"type": "text",
			"version": 74,
			"versionNonce": 420200887,
			"isDeleted": false,
			"id": "RpCf6rN5",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -190.33331298828125,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 161,
			"height": 35,
			"seed": 851220887,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"YO5hxAKxxJRWBVrEFuZdm"
			],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "hidden layer",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "hidden layer"
		},
		{
			"type": "arrow",
			"version": 133,
			"versionNonce": 533961017,
			"isDeleted": false,
			"id": "oSGYV9jfhe0vAHgLPzSfw",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 49,
			"y": -318.5,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 108.87035354788611,
			"height": 108.16689711480237,
			"seed": 1478799193,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-108.87035354788611,
					108.16689711480237
				]
			]
		},
		{
			"type": "text",
			"version": 44,
			"versionNonce": 733508311,
			"isDeleted": false,
			"id": "pGGYuNZa",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 66,
			"y": -340,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 198,
			"height": 35,
			"seed": 759385783,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "input sequence",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "input sequence"
		},
		{
			"type": "arrow",
			"version": 138,
			"versionNonce": 1616775705,
			"isDeleted": false,
			"id": "dZwulEwk8Tnh3m-G4SHON",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 180,
			"y": 69.66668701171875,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 220,
			"height": 0,
			"seed": 912743481,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": {
				"elementId": "GjGV-yHyOmNiEDcs2RY_Q",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-220,
					0
				]
			]
		},
		{
			"type": "text",
			"version": 77,
			"versionNonce": 1676804087,
			"isDeleted": false,
			"id": "qNFsKwcS",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 197,
			"y": 55.16668701171875,
			"strokeColor": "#343a40",
			"backgroundColor": "#868e96",
			"width": 249,
			"height": 35,
			"seed": 1266048983,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "response sequence",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "response sequence"
		},
		{
			"type": "arrow",
			"version": 365,
			"versionNonce": 203906809,
			"isDeleted": false,
			"id": "EmhoZ_XTk95ed8A9kP0u-",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -139.22688208900394,
			"y": -47.75165460430085,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 58.897111086174846,
			"height": 1.6359685526788255,
			"seed": 741280535,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "x0Tg5ziLGlxFkIhexUO7t",
				"focus": 0.1578724817850979,
				"gap": 1
			},
			"endBinding": {
				"elementId": "n4W9QAKIf8T66ngtaxNLO",
				"focus": -0.019042429270265383,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58.897111086174846,
					-1.6359685526788255
				]
			]
		},
		{
			"type": "arrow",
			"version": 117,
			"versionNonce": 868018455,
			"isDeleted": false,
			"id": "S2gzCHtzXYCAwzMPb8zoo",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -40,
			"y": -50.33331298828125,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 60,
			"height": 0,
			"seed": 786290617,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "n4W9QAKIf8T66ngtaxNLO",
				"focus": 0,
				"gap": 1
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 129,
			"versionNonce": 565385177,
			"isDeleted": false,
			"id": "7mmieKGz7lH4cPAsSyeQF",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -358.85027359971775,
			"y": -49.23221246885955,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 78.85027359971775,
			"height": 1.1011005194216992,
			"seed": 1637322649,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": {
				"elementId": "XAfkJc4lvHn6_VWKOwVyM",
				"focus": 0.013963086294628892,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					78.85027359971775,
					-1.1011005194216992
				]
			]
		},
		{
			"type": "ellipse",
			"version": 121,
			"versionNonce": 1485680183,
			"isDeleted": false,
			"id": "o3sKoboi_4YXC1zQiypu_",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -860,
			"y": -60,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1351612441,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"ddsDWHT4fQ7WxlbOaT1Vz",
				"Bm4RFb3NYodnarIyv4k3V",
				"7mmieKGz7lH4cPAsSyeQF",
				"Ap_5sLZRbEXuwrjVIop6S",
				"Jni41omPrxBeE9XGXxow7",
				"s2GyoulFcqr1BuLQ3trfq",
				"0tUtKZ1FBtoovyVtm2voh",
				"kQpYHnKDtAKZSfDJiK5IK",
				"bSiqf3DZ5OFC80iJld7h3"
			]
		},
		{
			"type": "arrow",
			"version": 59,
			"versionNonce": 51688633,
			"isDeleted": false,
			"id": "Ap_5sLZRbEXuwrjVIop6S",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -840,
			"y": -140,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 0,
			"height": 80,
			"seed": 931287607,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "zKKAJKCNqRrQz-379m5LH",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "o3sKoboi_4YXC1zQiypu_",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 58,
			"versionNonce": 1833488215,
			"isDeleted": false,
			"id": "Jni41omPrxBeE9XGXxow7",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -840,
			"y": -20,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 0,
			"height": 80,
			"seed": 1019261433,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "o3sKoboi_4YXC1zQiypu_",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "XHPtXnErXKxY_MOGe0WTB",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 224,
			"versionNonce": 66041241,
			"isDeleted": false,
			"id": "kQpYHnKDtAKZSfDJiK5IK",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -820,
			"y": -60,
			"strokeColor": "#2b8a3e",
			"backgroundColor": "transparent",
			"width": 72,
			"height": 110.66668701171875,
			"seed": 35755545,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "o3sKoboi_4YXC1zQiypu_",
				"focus": 0.23955573269050806,
				"gap": 8.284271247461902
			},
			"endBinding": {
				"elementId": "o3sKoboi_4YXC1zQiypu_",
				"focus": -0.4120319459119864,
				"gap": 12.509864705508583
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					35.33331298828125,
					-50
				],
				[
					72,
					4
				],
				[
					70,
					49.3333740234375
				],
				[
					36,
					60.66668701171875
				],
				[
					8.66668701171875,
					35.3333740234375
				]
			]
		},
		{
			"type": "arrow",
			"version": 24,
			"versionNonce": 1011010679,
			"isDeleted": false,
			"id": "bSiqf3DZ5OFC80iJld7h3",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -960,
			"y": 40,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 100,
			"height": 60,
			"seed": 633276055,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "en0jDMDO",
				"focus": -0.43127962085308064,
				"gap": 1
			},
			"endBinding": {
				"elementId": "o3sKoboi_4YXC1zQiypu_",
				"focus": -0.3429971702850177,
				"gap": 8.284271247461902
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					100,
					-60
				]
			]
		},
		{
			"type": "text",
			"version": 29,
			"versionNonce": 930585209,
			"isDeleted": false,
			"id": "en0jDMDO",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -1000,
			"y": 40,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 99,
			"height": 25,
			"seed": 1825579319,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"bSiqf3DZ5OFC80iJld7h3"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "activation",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "activation"
		},
		{
			"type": "text",
			"version": 67,
			"versionNonce": 660187383,
			"isDeleted": false,
			"id": "lrjg188r",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -1100,
			"y": -520,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 634,
			"height": 120,
			"seed": 1543100089,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 94.81481481481481,
			"fontFamily": 1,
			"text": "VANILLA RNN",
			"baseline": 84,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "VANILLA RNN"
		},
		{
			"type": "text",
			"version": 106,
			"versionNonce": 2121261913,
			"isDeleted": false,
			"id": "51q7S0hL",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -880,
			"y": 1080,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 399,
			"height": 35,
			"seed": 440804857,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Some Problem OF vanilla RNN",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "Some Problem OF vanilla RNN"
		},
		{
			"type": "text",
			"version": 158,
			"versionNonce": 1265982135,
			"isDeleted": false,
			"id": "kNPMYtPp",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -840,
			"y": 1140,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 667,
			"height": 29,
			"seed": 1926923897,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "👉1. only take information from the past not the future see BRNN",
			"baseline": 22,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "1. only take information from the past not the future see [[BRNN]]"
		},
		{
			"type": "ellipse",
			"version": 163,
			"versionNonce": 1428951097,
			"isDeleted": false,
			"id": "U-sCZM1sn-XM1L5W7hQBW",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -879.8514709697479,
			"y": 300,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 898474519,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"0kuFnGa1pbKqvThRl-8Gi",
				"n_QeUBSmNSBytxFsnZpyY"
			]
		},
		{
			"type": "ellipse",
			"version": 162,
			"versionNonce": 1871113175,
			"isDeleted": false,
			"id": "s9U-XWwFTTgBigZmLv_4O",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -779.8514709697479,
			"y": 300,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 1831235289,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"rPd8raLHAArIzfQYC5w0d",
				"SCXBLiadRiysI78nh7duW",
				"w6lFKkQTJbW8WV_jFidkT"
			]
		},
		{
			"type": "ellipse",
			"version": 181,
			"versionNonce": 1636802841,
			"isDeleted": false,
			"id": "DrapAGIZn-IcdMCoSeD17",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -679.8514709697479,
			"y": 300,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 836716343,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"M2WCS9EdUN9DmBkQTrnl7",
				"rPd8raLHAArIzfQYC5w0d",
				"AqSj_XCLbCdp1U1E8P_br",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"oSGYV9jfhe0vAHgLPzSfw"
			]
		},
		{
			"type": "arrow",
			"version": 494,
			"versionNonce": 2039044343,
			"isDeleted": false,
			"id": "n_QeUBSmNSBytxFsnZpyY",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -859.132157290534,
			"y": 340.9876770470411,
			"strokeColor": "#e67700",
			"backgroundColor": "#4c6ef5",
			"width": 0.6854246593804874,
			"height": 78.01235029739729,
			"seed": 1409625017,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "U-sCZM1sn-XM1L5W7hQBW",
				"focus": -0.04518393614519731,
				"gap": 1
			},
			"endBinding": {
				"elementId": "sA7dLndApD07Htvs7LWhW",
				"focus": -0.007530656024205806,
				"gap": 1.000000000000039
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.6854246593804874,
					78.01235029739729
				]
			]
		},
		{
			"type": "ellipse",
			"version": 160,
			"versionNonce": 1091442169,
			"isDeleted": false,
			"id": "sA7dLndApD07Htvs7LWhW",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -879.8514709697479,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 989808727,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"n_QeUBSmNSBytxFsnZpyY",
				"BRfQkwd-RKp-WxVjzX0Ty",
				"w6lFKkQTJbW8WV_jFidkT"
			]
		},
		{
			"type": "ellipse",
			"version": 167,
			"versionNonce": 788603415,
			"isDeleted": false,
			"id": "HnpZcQS8yv7Vt0Jk2O4Zs",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -779.8514709697479,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1840860313,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"n_QeUBSmNSBytxFsnZpyY",
				"SCXBLiadRiysI78nh7duW",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"w6lFKkQTJbW8WV_jFidkT",
				"hoj68AXqbw1E-4J3nxj2B",
				"JZ0fKNvMGP9OFH3rURWfC"
			]
		},
		{
			"type": "ellipse",
			"version": 170,
			"versionNonce": 380301017,
			"isDeleted": false,
			"id": "5NYUDQJ_BoEK2vx-SN_mR",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -680,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1589781879,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"n_QeUBSmNSBytxFsnZpyY",
				"AqSj_XCLbCdp1U1E8P_br",
				"IkNd9d72O5JgKBplCKczo",
				"hoj68AXqbw1E-4J3nxj2B",
				"988RBYNz_MCH-ONWR4zs3",
				"nhOQmF7GIY6DIzYXqxUkc"
			]
		},
		{
			"type": "arrow",
			"version": 450,
			"versionNonce": 820168503,
			"isDeleted": false,
			"id": "SCXBLiadRiysI78nh7duW",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -759.8514709697479,
			"y": 340,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 32919929,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "s9U-XWwFTTgBigZmLv_4O",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "HnpZcQS8yv7Vt0Jk2O4Zs",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 516,
			"versionNonce": 1492757433,
			"isDeleted": false,
			"id": "AqSj_XCLbCdp1U1E8P_br",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -659.8774635294468,
			"y": 340.9999839139662,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0.09654391002413831,
			"height": 78.00003217206853,
			"seed": 2087476887,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "DrapAGIZn-IcdMCoSeD17",
				"focus": -4.1505137554864e-11,
				"gap": 1
			},
			"endBinding": {
				"elementId": "5NYUDQJ_BoEK2vx-SN_mR",
				"focus": 1.9548442373360566e-15,
				"gap": 1.0000000000000462
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.09654391002413831,
					78.00003217206853
				]
			]
		},
		{
			"type": "arrow",
			"version": 569,
			"versionNonce": 227723351,
			"isDeleted": false,
			"id": "w6lFKkQTJbW8WV_jFidkT",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -839.8514709697479,
			"y": 440,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 60,
			"height": 0,
			"seed": 369080921,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "sA7dLndApD07Htvs7LWhW",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "HnpZcQS8yv7Vt0Jk2O4Zs",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 625,
			"versionNonce": 1612565657,
			"isDeleted": false,
			"id": "hoj68AXqbw1E-4J3nxj2B",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -739.0104256203088,
			"y": 442.5789200729412,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 58.03258915272147,
			"height": 1.6143594791133182,
			"seed": 969464599,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "HnpZcQS8yv7Vt0Jk2O4Zs",
				"focus": 0.1578729005545167,
				"gap": 1
			},
			"endBinding": {
				"elementId": "5NYUDQJ_BoEK2vx-SN_mR",
				"focus": -0.019042429270267162,
				"gap": 1.000000000000007
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58.03258915272147,
					-1.6143594791133182
				]
			]
		},
		{
			"type": "arrow",
			"version": 445,
			"versionNonce": 317039991,
			"isDeleted": false,
			"id": "988RBYNz_MCH-ONWR4zs3",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -639,
			"y": 440,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 58,
			"height": 0,
			"seed": 1209484761,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "5NYUDQJ_BoEK2vx-SN_mR",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Qn5tF0bOgxgATGwIvAoXX",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 134,
			"versionNonce": 868929913,
			"isDeleted": false,
			"id": "xPu3gTw93p1quL-NxImGo",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -958.7017445694655,
			"y": 441.1011005194217,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 78.85027359971775,
			"height": 1.1011005194216992,
			"seed": 1548443703,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					78.85027359971775,
					-1.1011005194216992
				]
			]
		},
		{
			"type": "ellipse",
			"version": 159,
			"versionNonce": 1612701335,
			"isDeleted": false,
			"id": "Qn5tF0bOgxgATGwIvAoXX",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -580,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 1116011353,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"1e6iGAnEz-qqChqmzbzYy",
				"wUaBrW66ZXV8jO822IzZK",
				"dZwulEwk8Tnh3m-G4SHON",
				"988RBYNz_MCH-ONWR4zs3"
			]
		},
		{
			"type": "text",
			"version": 130,
			"versionNonce": 423901785,
			"isDeleted": false,
			"id": "J7dLydJz",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -900,
			"y": 500,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 359,
			"height": 35,
			"seed": 1041272631,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Many-To-One Architecture",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "Many-To-One Architecture"
		},
		{
			"type": "text",
			"version": 148,
			"versionNonce": 938865591,
			"isDeleted": false,
			"id": "GW6WfaUZ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -320,
			"y": 149.66668701171875,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 370,
			"height": 35,
			"seed": 585702905,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "Many-to-Many Architecture",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "Many-to-Many Architecture"
		},
		{
			"type": "ellipse",
			"version": 135,
			"versionNonce": 1633519417,
			"isDeleted": false,
			"id": "pTezLILUY3Ae4l7pA52FV",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -300.5067825624348,
			"y": 300,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 810656665,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"0kuFnGa1pbKqvThRl-8Gi",
				"4-5jRcAOTSoq6rdXhbnpm"
			]
		},
		{
			"type": "arrow",
			"version": 347,
			"versionNonce": 311190743,
			"isDeleted": false,
			"id": "4-5jRcAOTSoq6rdXhbnpm",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -279.7874688832209,
			"y": 340.9876770470411,
			"strokeColor": "#e67700",
			"backgroundColor": "#4c6ef5",
			"width": 0.6854246593804874,
			"height": 78.01235029739729,
			"seed": 376297367,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "pTezLILUY3Ae4l7pA52FV",
				"focus": -0.04518393614519731,
				"gap": 1
			},
			"endBinding": {
				"elementId": "2FrYGOl4Cm3UCEwk2abCB",
				"focus": -0.007530656024205806,
				"gap": 1.000000000000039
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.6854246593804874,
					78.01235029739729
				]
			]
		},
		{
			"type": "ellipse",
			"version": 142,
			"versionNonce": 1987721241,
			"isDeleted": false,
			"id": "2FrYGOl4Cm3UCEwk2abCB",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -300.5067825624348,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 833507673,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"M_eWA3VCiF-Z46QtQCBMN",
				"T5dxC1FNTf_M13EUTDBok"
			]
		},
		{
			"type": "ellipse",
			"version": 151,
			"versionNonce": 1870115319,
			"isDeleted": false,
			"id": "w4z6M5w7Kro4U4D6o-kjL",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -200.00000000000006,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1142738103,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"upG25wLT_Ba5B8txN3K13",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"T5dxC1FNTf_M13EUTDBok",
				"r5E-4_ndTFjZ8hyfoaUml",
				"uUxghp9witMPd7bsjPInX",
				"QeDWNsSSVF7FAok1aDdMp"
			]
		},
		{
			"type": "ellipse",
			"version": 142,
			"versionNonce": 713936121,
			"isDeleted": false,
			"id": "Nj8mJ8m5wpcEZ0fPqmjtr",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -100.50678256243481,
			"y": 420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 845987385,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"JOSVI9ccIOHsqPStskF80",
				"IkNd9d72O5JgKBplCKczo",
				"r5E-4_ndTFjZ8hyfoaUml",
				"laP3M_jjLqCCdtwrFzYGr",
				"-jYwJH3oYb9Xhf0pUumxw",
				"FgKyXPaNeB9ortSa2DIgm"
			]
		},
		{
			"type": "arrow",
			"version": 519,
			"versionNonce": 1740397335,
			"isDeleted": false,
			"id": "T5dxC1FNTf_M13EUTDBok",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -259.50678256243583,
			"y": 440.0000067266253,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 58.50678256243589,
			"height": 0.0000049499324177304516,
			"seed": 8145655,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "2FrYGOl4Cm3UCEwk2abCB",
				"focus": 4.2516590700390256e-7,
				"gap": 1.0000000000000533
			},
			"endBinding": {
				"elementId": "w4z6M5w7Kro4U4D6o-kjL",
				"focus": -3.6518702381446866e-15,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58.50678256243589,
					-0.0000049499324177304516
				]
			]
		},
		{
			"type": "arrow",
			"version": 303,
			"versionNonce": 1159448025,
			"isDeleted": false,
			"id": "M_eWA3VCiF-Z46QtQCBMN",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -280.5067825624348,
			"y": 460,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 209404921,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "2FrYGOl4Cm3UCEwk2abCB",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "mFuLNtrLujVaeN1Y-FCky",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 123,
			"versionNonce": 2144264247,
			"isDeleted": false,
			"id": "mFuLNtrLujVaeN1Y-FCky",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -300.5067825624348,
			"y": 540,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 2126894103,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"M_eWA3VCiF-Z46QtQCBMN",
				"QeDWNsSSVF7FAok1aDdMp"
			]
		},
		{
			"type": "ellipse",
			"version": 119,
			"versionNonce": 1362600633,
			"isDeleted": false,
			"id": "k1OLQiMv2um6m_of89H_v",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -200.5067825624348,
			"y": 540,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 407002329,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"uUxghp9witMPd7bsjPInX",
				"T5dxC1FNTf_M13EUTDBok",
				"FgKyXPaNeB9ortSa2DIgm"
			]
		},
		{
			"type": "arrow",
			"version": 321,
			"versionNonce": 965984599,
			"isDeleted": false,
			"id": "uUxghp9witMPd7bsjPInX",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -180.08868615686293,
			"y": 460.99981273167884,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0.3294102478679406,
			"height": 78.00037453664584,
			"seed": 1229011255,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "w4z6M5w7Kro4U4D6o-kjL",
				"focus": -4.205230959613341e-11,
				"gap": 1.000000000000007
			},
			"endBinding": {
				"elementId": "k1OLQiMv2um6m_of89H_v",
				"focus": 8.209844296569274e-16,
				"gap": 1.0000000000000213
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.3294102478679406,
					78.00037453664584
				]
			]
		},
		{
			"type": "arrow",
			"version": 265,
			"versionNonce": 730129305,
			"isDeleted": false,
			"id": "-jYwJH3oYb9Xhf0pUumxw",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -80.50678256243481,
			"y": 460,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1366514105,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Nj8mJ8m5wpcEZ0fPqmjtr",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "S77kJvWwsacr_2wwt7kgv",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 122,
			"versionNonce": 1717534327,
			"isDeleted": false,
			"id": "S77kJvWwsacr_2wwt7kgv",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -100.50678256243481,
			"y": 540,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 894630487,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"uUxghp9witMPd7bsjPInX",
				"-jYwJH3oYb9Xhf0pUumxw",
				"dZwulEwk8Tnh3m-G4SHON"
			]
		},
		{
			"type": "arrow",
			"version": 523,
			"versionNonce": 1345783929,
			"isDeleted": false,
			"id": "r5E-4_ndTFjZ8hyfoaUml",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -159.1586958427834,
			"y": 442.57682770599513,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 57.674173477135525,
			"height": 1.6101670888754143,
			"seed": 959560345,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "w4z6M5w7Kro4U4D6o-kjL",
				"focus": 0.15787260072796286,
				"gap": 1.000000000000007
			},
			"endBinding": {
				"elementId": "Nj8mJ8m5wpcEZ0fPqmjtr",
				"focus": -0.019042429270271756,
				"gap": 1.0000000000000036
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					57.674173477135525,
					-1.6101670888754143
				]
			]
		},
		{
			"type": "arrow",
			"version": 172,
			"versionNonce": 955684759,
			"isDeleted": false,
			"id": "laP3M_jjLqCCdtwrFzYGr",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -60.50678256243481,
			"y": 440,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 60,
			"height": 0,
			"seed": 2010093431,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Nj8mJ8m5wpcEZ0fPqmjtr",
				"focus": 0,
				"gap": 1
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 126,
			"versionNonce": 1762759001,
			"isDeleted": false,
			"id": "mqXKoeVlRBLhY2ftP87bj",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -379.35705616215245,
			"y": 441.1011005194217,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 78.85027359971775,
			"height": 1.1011005194216992,
			"seed": 1613823865,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					78.85027359971775,
					-1.1011005194216992
				]
			]
		},
		{
			"type": "text",
			"version": 176,
			"versionNonce": 395152567,
			"isDeleted": false,
			"id": "ulNP2QZB",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340.00000000000006,
			"y": 600,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 359,
			"height": 35,
			"seed": 1287754809,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "One-To-Many Architecture",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "One-To-Many Architecture"
		},
		{
			"type": "ellipse",
			"version": 199,
			"versionNonce": 459800121,
			"isDeleted": false,
			"id": "XxyqtWXpdMYDBVgFFV6Q4",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -780.3349362487907,
			"y": 660,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 1947654103,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"0kuFnGa1pbKqvThRl-8Gi",
				"rdVHxwfH5leDplb7Q72kg"
			]
		},
		{
			"type": "ellipse",
			"version": 198,
			"versionNonce": 768138711,
			"isDeleted": false,
			"id": "vK0HywDTT71--7QqCrSW_",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -680.3349362487907,
			"y": 660,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 524580121,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"NOpYf4jpPrI81FHs44seG",
				"rPd8raLHAArIzfQYC5w0d",
				"5iDxaij9Kefr9PRuI6IzW",
				"CdVHORBEVMVIy5RxPRLWM"
			]
		},
		{
			"type": "ellipse",
			"version": 217,
			"versionNonce": 371052313,
			"isDeleted": false,
			"id": "w_LoRr4dwMk2i1rlm_eJ7",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -580.3349362487907,
			"y": 660,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 1027327223,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"M2WCS9EdUN9DmBkQTrnl7",
				"rPd8raLHAArIzfQYC5w0d",
				"KabNn9Xmzac_Skqg6QHa4",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"oSGYV9jfhe0vAHgLPzSfw"
			]
		},
		{
			"type": "arrow",
			"version": 902,
			"versionNonce": 1052576503,
			"isDeleted": false,
			"id": "rdVHxwfH5leDplb7Q72kg",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -759.6156225695768,
			"y": 700.987677047041,
			"strokeColor": "#e67700",
			"backgroundColor": "#4c6ef5",
			"width": 0.6854246593804874,
			"height": 78.01235029739729,
			"seed": 419981817,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XxyqtWXpdMYDBVgFFV6Q4",
				"focus": -0.04518393614519727,
				"gap": 1
			},
			"endBinding": {
				"elementId": "TQaza4K8SkTSivBu02TGX",
				"focus": -0.007530656024205888,
				"gap": 1.0000000000002096
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.6854246593804874,
					78.01235029739729
				]
			]
		},
		{
			"type": "ellipse",
			"version": 198,
			"versionNonce": 1604748281,
			"isDeleted": false,
			"id": "TQaza4K8SkTSivBu02TGX",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -780.3349362487907,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 696651287,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"rdVHxwfH5leDplb7Q72kg",
				"BRfQkwd-RKp-WxVjzX0Ty",
				"CdVHORBEVMVIy5RxPRLWM"
			]
		},
		{
			"type": "ellipse",
			"version": 207,
			"versionNonce": 921605143,
			"isDeleted": false,
			"id": "4_2s-9ikW0B14LGgQVkU4",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -680.3349362487907,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 607830745,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"rdVHxwfH5leDplb7Q72kg",
				"5iDxaij9Kefr9PRuI6IzW",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"CdVHORBEVMVIy5RxPRLWM",
				"mCoKScmILiHUVRcktCDHw",
				"JZ0fKNvMGP9OFH3rURWfC"
			]
		},
		{
			"type": "ellipse",
			"version": 210,
			"versionNonce": 1678089433,
			"isDeleted": false,
			"id": "Vwu0oBg6hTig7yFD0rCRl",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -580.4834652790428,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1676292919,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"rdVHxwfH5leDplb7Q72kg",
				"KabNn9Xmzac_Skqg6QHa4",
				"IkNd9d72O5JgKBplCKczo",
				"mCoKScmILiHUVRcktCDHw",
				"LR1d3_KwdB12rCN8CKOjm",
				"nhOQmF7GIY6DIzYXqxUkc"
			]
		},
		{
			"type": "arrow",
			"version": 858,
			"versionNonce": 2119301431,
			"isDeleted": false,
			"id": "5iDxaij9Kefr9PRuI6IzW",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -660.3349362487907,
			"y": 700,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1299120057,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "vK0HywDTT71--7QqCrSW_",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "4_2s-9ikW0B14LGgQVkU4",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "arrow",
			"version": 924,
			"versionNonce": 1120836025,
			"isDeleted": false,
			"id": "KabNn9Xmzac_Skqg6QHa4",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -560.3609288084895,
			"y": 700.9999839139662,
			"strokeColor": "#e67700",
			"backgroundColor": "#868e96",
			"width": 0.09654391002413831,
			"height": 78.00003217206853,
			"seed": 1154266199,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "w_LoRr4dwMk2i1rlm_eJ7",
				"focus": -4.151081733529551e-11,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Vwu0oBg6hTig7yFD0rCRl",
				"focus": 7.64031811356668e-15,
				"gap": 1.0000000000000462
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.09654391002413831,
					78.00003217206853
				]
			]
		},
		{
			"type": "arrow",
			"version": 977,
			"versionNonce": 172735063,
			"isDeleted": false,
			"id": "CdVHORBEVMVIy5RxPRLWM",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -740.3349362487907,
			"y": 800,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 60,
			"height": 0,
			"seed": 1120444569,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "TQaza4K8SkTSivBu02TGX",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "4_2s-9ikW0B14LGgQVkU4",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 1033,
			"versionNonce": 1562704537,
			"isDeleted": false,
			"id": "mCoKScmILiHUVRcktCDHw",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -639.4938908993515,
			"y": 802.5789200729412,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 58.03258915272147,
			"height": 1.6143594791133182,
			"seed": 604115319,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "4_2s-9ikW0B14LGgQVkU4",
				"focus": 0.1578729005545167,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Vwu0oBg6hTig7yFD0rCRl",
				"focus": -0.019042429270267162,
				"gap": 1.000000000000007
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58.03258915272147,
					-1.6143594791133182
				]
			]
		},
		{
			"type": "arrow",
			"version": 853,
			"versionNonce": 800085879,
			"isDeleted": false,
			"id": "LR1d3_KwdB12rCN8CKOjm",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -539.4834652790428,
			"y": 800,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 58,
			"height": 0,
			"seed": 570107257,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Vwu0oBg6hTig7yFD0rCRl",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "qgwJA4xoLfmB2bLg6ajrt",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 167,
			"versionNonce": 599539577,
			"isDeleted": false,
			"id": "7Vr7K3Q90xxlXYYOejc2a",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -859.1852098485083,
			"y": 801.1011005194218,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 78.85027359971775,
			"height": 1.1011005194216992,
			"seed": 1791937175,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": null,
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					78.85027359971775,
					-1.1011005194216992
				]
			]
		},
		{
			"type": "ellipse",
			"version": 198,
			"versionNonce": 885280919,
			"isDeleted": false,
			"id": "qgwJA4xoLfmB2bLg6ajrt",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -480.4834652790428,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#12b886",
			"width": 40,
			"height": 40,
			"seed": 105516633,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"1e6iGAnEz-qqChqmzbzYy",
				"wUaBrW66ZXV8jO822IzZK",
				"dZwulEwk8Tnh3m-G4SHON",
				"LR1d3_KwdB12rCN8CKOjm",
				"FhNpsH3VyG0K-ct5OgFVD"
			]
		},
		{
			"type": "ellipse",
			"version": 181,
			"versionNonce": 618129497,
			"isDeleted": false,
			"id": "rQaMebVF7igAXMsReJTyJ",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360.54719481406755,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 602894457,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"G4-uHq0Gb5ZRtuVt4veiL",
				"APp2ZDvHL-BRJcdCo00gi"
			]
		},
		{
			"type": "ellipse",
			"version": 192,
			"versionNonce": 591144375,
			"isDeleted": false,
			"id": "5bJoth2e6xsJCrvkCph0K",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -260.04041225163263,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 1767419799,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"upG25wLT_Ba5B8txN3K13",
				"Nq9w3IvVQb_WWrTBkIgu_",
				"APp2ZDvHL-BRJcdCo00gi",
				"D8o2vyAY8ZaUVtSn5VAEF",
				"v5xYppjGL9f2xoJaPvzf7",
				"QuQZvVPz9-dELSd-rg1z6"
			]
		},
		{
			"type": "ellipse",
			"version": 184,
			"versionNonce": 1514894649,
			"isDeleted": false,
			"id": "h40sKJpNhanjgjGcxXyJY",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160.5471948140675,
			"y": 780,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 2038233433,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"4-5jRcAOTSoq6rdXhbnpm",
				"JOSVI9ccIOHsqPStskF80",
				"IkNd9d72O5JgKBplCKczo",
				"D8o2vyAY8ZaUVtSn5VAEF",
				"gpqbwGXhUW22HJ9VQHYFK",
				"aTZn1u3VL_5mK6iBQdpFk",
				"KNoJDh_eSltE2FiiiI6AL"
			]
		},
		{
			"type": "arrow",
			"version": 950,
			"versionNonce": 1513903831,
			"isDeleted": false,
			"id": "APp2ZDvHL-BRJcdCo00gi",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -319.54719481406846,
			"y": 800.0000067266253,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 58.50678256243589,
			"height": 0.0000049499324177304516,
			"seed": 602711223,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "rQaMebVF7igAXMsReJTyJ",
				"focus": 4.251659070039031e-7,
				"gap": 1.000000000000167
			},
			"endBinding": {
				"elementId": "5bJoth2e6xsJCrvkCph0K",
				"focus": -3.651870006504413e-15,
				"gap": 1.0000000000000178
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					58.50678256243589,
					-0.0000049499324177304516
				]
			]
		},
		{
			"type": "arrow",
			"version": 734,
			"versionNonce": 930768409,
			"isDeleted": false,
			"id": "G4-uHq0Gb5ZRtuVt4veiL",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340.54719481406755,
			"y": 820,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 1512465977,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "rQaMebVF7igAXMsReJTyJ",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "bnomobvmrdcrZVtSwEk-P",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 160,
			"versionNonce": 1616471031,
			"isDeleted": false,
			"id": "bnomobvmrdcrZVtSwEk-P",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360.54719481406755,
			"y": 900,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 243594711,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"G4-uHq0Gb5ZRtuVt4veiL",
				"QuQZvVPz9-dELSd-rg1z6"
			]
		},
		{
			"type": "ellipse",
			"version": 156,
			"versionNonce": 2002270969,
			"isDeleted": false,
			"id": "bHj7KdxPTjra9V1thuecg",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -260.5471948140675,
			"y": 900,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 1087321881,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"v5xYppjGL9f2xoJaPvzf7",
				"APp2ZDvHL-BRJcdCo00gi",
				"KNoJDh_eSltE2FiiiI6AL"
			]
		},
		{
			"type": "arrow",
			"version": 752,
			"versionNonce": 1972882711,
			"isDeleted": false,
			"id": "v5xYppjGL9f2xoJaPvzf7",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -240.1290984084955,
			"y": 820.999812731679,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0.3294102478679406,
			"height": 78.00037453664584,
			"seed": 1614160631,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "5bJoth2e6xsJCrvkCph0K",
				"focus": -4.205233350830126e-11,
				"gap": 1.0000000000001208
			},
			"endBinding": {
				"elementId": "bHj7KdxPTjra9V1thuecg",
				"focus": 6.5291604988249945e-15,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-0.3294102478679406,
					78.00037453664584
				]
			]
		},
		{
			"type": "arrow",
			"version": 696,
			"versionNonce": 711488473,
			"isDeleted": false,
			"id": "aTZn1u3VL_5mK6iBQdpFk",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -140.5471948140675,
			"y": 820,
			"strokeColor": "#364fc7",
			"backgroundColor": "#868e96",
			"width": 0,
			"height": 80,
			"seed": 2146102265,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "h40sKJpNhanjgjGcxXyJY",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "6F4u0r82NDS9Y4SdvD1V9",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					80
				]
			]
		},
		{
			"type": "ellipse",
			"version": 159,
			"versionNonce": 1701581367,
			"isDeleted": false,
			"id": "6F4u0r82NDS9Y4SdvD1V9",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -160.5471948140675,
			"y": 900,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 2038737943,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"v5xYppjGL9f2xoJaPvzf7",
				"aTZn1u3VL_5mK6iBQdpFk",
				"dZwulEwk8Tnh3m-G4SHON"
			]
		},
		{
			"type": "arrow",
			"version": 954,
			"versionNonce": 79597753,
			"isDeleted": false,
			"id": "D8o2vyAY8ZaUVtSn5VAEF",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -219.19910809441598,
			"y": 802.5768277059951,
			"strokeColor": "#5c940d",
			"backgroundColor": "#868e96",
			"width": 57.674173477135525,
			"height": 1.6101670888754143,
			"seed": 1333593305,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "5bJoth2e6xsJCrvkCph0K",
				"focus": 0.15787260072796286,
				"gap": 1.000000000000007
			},
			"endBinding": {
				"elementId": "h40sKJpNhanjgjGcxXyJY",
				"focus": -0.019042429270271933,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					57.674173477135525,
					-1.6101670888754143
				]
			]
		},
		{
			"type": "arrow",
			"version": 405,
			"versionNonce": 414888791,
			"isDeleted": false,
			"id": "gpqbwGXhUW22HJ9VQHYFK",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -120.5471948140675,
			"y": 800,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 60,
			"height": 0,
			"seed": 912593207,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "h40sKJpNhanjgjGcxXyJY",
				"focus": 0,
				"gap": 1
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					60,
					0
				]
			]
		},
		{
			"type": "arrow",
			"version": 207,
			"versionNonce": 1811812761,
			"isDeleted": false,
			"id": "FhNpsH3VyG0K-ct5OgFVD",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -439.3974684137851,
			"y": 801.1011005194218,
			"strokeColor": "#5c940d",
			"backgroundColor": "transparent",
			"width": 78.85027359971775,
			"height": 1.1011005194216992,
			"seed": 454623673,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "qgwJA4xoLfmB2bLg6ajrt",
				"focus": 0.06977093842329828,
				"gap": 1.1147267601437
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					78.85027359971775,
					-1.1011005194216992
				]
			]
		},
		{
			"type": "rectangle",
			"version": 37,
			"versionNonce": 1742221431,
			"isDeleted": false,
			"id": "Qaio4hnbWJC1U51_Ukc9f",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -900,
			"y": 620,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 480,
			"height": 260,
			"seed": 1716688087,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": []
		},
		{
			"type": "text",
			"version": 23,
			"versionNonce": 1163556473,
			"isDeleted": false,
			"id": "8l9xUD43",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -720,
			"y": 840,
			"strokeColor": "#c92a2a",
			"backgroundColor": "transparent",
			"width": 135,
			"height": 35,
			"seed": 494506999,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "ENCODER",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "ENCODER"
		},
		{
			"type": "rectangle",
			"version": 48,
			"versionNonce": 747178391,
			"isDeleted": false,
			"id": "dRHO-fvDQKPFAh7ALVU2E",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -500,
			"y": 740,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 480,
			"height": 280,
			"seed": 1292747257,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": []
		},
		{
			"type": "text",
			"version": 24,
			"versionNonce": 60243801,
			"isDeleted": false,
			"id": "WdDzHPx5",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -300,
			"y": 960,
			"strokeColor": "#364fc7",
			"backgroundColor": "transparent",
			"width": 139,
			"height": 35,
			"seed": 14052055,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "DECODER",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "DECODER"
		},
		{
			"type": "arrow",
			"version": 187,
			"versionNonce": 335658679,
			"isDeleted": false,
			"id": "QeDWNsSSVF7FAok1aDdMp",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -260,
			"y": 560,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 71.23813592819988,
			"height": 242.1341904148909,
			"seed": 75421143,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "mFuLNtrLujVaeN1Y-FCky",
				"focus": 0.377553813854241,
				"gap": 1
			},
			"endBinding": {
				"elementId": "w4z6M5w7Kro4U4D6o-kjL",
				"focus": 0.23425889086995863,
				"gap": 12.664597855572477
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					23.90476190476238,
					-9.467503403172145
				],
				[
					31.23813592819988,
					-242.1341904148909
				],
				[
					71.23813592819988,
					-151.46753392075027
				]
			]
		},
		{
			"type": "arrow",
			"version": 195,
			"versionNonce": 1963453497,
			"isDeleted": false,
			"id": "FgKyXPaNeB9ortSa2DIgm",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -159.22680218705906,
			"y": 566.198226548364,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 71.23813592819988,
			"height": 242.1341904148909,
			"seed": 1646362521,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "k1OLQiMv2um6m_of89H_v",
				"focus": 0.6799254282486562,
				"gap": 2.1642860728971165
			},
			"endBinding": {
				"elementId": "Nj8mJ8m5wpcEZ0fPqmjtr",
				"focus": 0.16772004738659546,
				"gap": 6.353680553709303
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					23.90476190476238,
					-9.467503403172145
				],
				[
					31.23813592819988,
					-242.1341904148909
				],
				[
					71.23813592819988,
					-151.46753392075027
				]
			]
		},
		{
			"type": "arrow",
			"version": 38,
			"versionNonce": 820432855,
			"isDeleted": false,
			"id": "QuQZvVPz9-dELSd-rg1z6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -320,
			"y": 900,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 61.23813592819988,
			"height": 81.46750340317215,
			"seed": 816979255,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "bnomobvmrdcrZVtSwEk-P",
				"focus": 0.22035881807083252,
				"gap": 8.673807119516663
			},
			"endBinding": {
				"elementId": "5bJoth2e6xsJCrvkCph0K",
				"focus": 0.1914766967166027,
				"gap": 6.342858393999094
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					61.23813592819988,
					-81.46750340317215
				]
			]
		},
		{
			"type": "arrow",
			"version": 48,
			"versionNonce": 1368390937,
			"isDeleted": false,
			"id": "KNoJDh_eSltE2FiiiI6AL",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -220,
			"y": 900,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 56.57144891648113,
			"height": 82.1341904148909,
			"seed": 2060734551,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "bHj7KdxPTjra9V1thuecg",
				"focus": 0.27884807557671576,
				"gap": 8.673807119516624
			},
			"endBinding": {
				"elementId": "h40sKJpNhanjgjGcxXyJY",
				"focus": 0.4354931619219986,
				"gap": 9.030046793984638
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					56.57144891648113,
					-82.1341904148909
				]
			]
		},
		{
			"type": "text",
			"version": 28,
			"versionNonce": 1296874743,
			"isDeleted": false,
			"id": "3wBFZu4k",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -720,
			"y": 920,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 148,
			"height": 45,
			"seed": 394760567,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 36,
			"fontFamily": 1,
			"text": "Seq2Seq",
			"baseline": 32,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "Seq2Seq"
		},
		{
			"type": "text",
			"version": 149,
			"versionNonce": 496909079,
			"isDeleted": false,
			"id": "FuZQcFqs",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -1060,
			"y": 2100,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 982,
			"height": 81,
			"seed": 796730780,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 64,
			"fontFamily": 1,
			"text": "LSTM (long short term memory)",
			"baseline": 57,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "LSTM (long short term memory)"
		},
		{
			"type": "text",
			"version": 38,
			"versionNonce": 2092434967,
			"isDeleted": false,
			"id": "GymB8DNZ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -600,
			"y": 100,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 125,
			"height": 25,
			"seed": 1625729495,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"h9cTDUjWAvN1T-PNB8qrE",
				"FDWzELII6uGCHlMbouIAi"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "hidden state",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "hidden state"
		},
		{
			"type": "arrow",
			"version": 38,
			"versionNonce": 1329875673,
			"isDeleted": false,
			"id": "h9cTDUjWAvN1T-PNB8qrE",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -580.3791523482246,
			"y": 99,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 159.62084765177542,
			"height": 119,
			"seed": 1599198615,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "GymB8DNZ",
				"focus": -0.3125,
				"gap": 1
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-159.62084765177542,
					-119
				]
			]
		},
		{
			"type": "arrow",
			"version": 42,
			"versionNonce": 1847405367,
			"isDeleted": false,
			"id": "FDWzELII6uGCHlMbouIAi",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -541.8653846153845,
			"y": 99,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 221.86538461538453,
			"height": 139,
			"seed": 1677925529,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "GymB8DNZ",
				"focus": -0.3142857142857142,
				"gap": 1
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					221.86538461538453,
					-139
				]
			]
		},
		{
			"type": "text",
			"version": 354,
			"versionNonce": 1022127449,
			"isDeleted": false,
			"id": "7zYkzFIs",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -960,
			"y": -320,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 485,
			"height": 88,
			"seed": 131745623,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 36,
			"fontFamily": 3,
			"text": "h(t) := f(h(t-1), x(t))\ny(t) := g(h(t))",
			"baseline": 79,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "h(t) := f(h(t-1), x(t))\ny(t) := g(h(t))"
		},
		{
			"type": "text",
			"version": 359,
			"versionNonce": 123718647,
			"isDeleted": false,
			"id": "6KEW8EUS",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 600,
			"y": -320,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 696,
			"height": 131,
			"seed": 1417926649,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Y41aZU_0yYHF9CBjWHqi-",
				"79pkQtWG_FvSc1iIik_Bc",
				"HWf8qR0azRcDfyphU6ip8"
			],
			"fontSize": 36,
			"fontFamily": 3,
			"text": "y(t) := h(t)\nh(t) := f( a(t) )\na(t) := U * x(t) + V * h(t-1) + b",
			"baseline": 123,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "y(t) := h(t)\nh(t) := f( a(t) )\na(t) := U * x(t) + V * h(t-1) + b"
		},
		{
			"type": "text",
			"version": 244,
			"versionNonce": 499540279,
			"isDeleted": false,
			"id": "Cgb8RTJT",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1240,
			"y": -380,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 185,
			"height": 25,
			"seed": 1382107639,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Wu8J_U5msRrki6TJbY7CO"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "activation function",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "activation function"
		},
		{
			"type": "arrow",
			"version": 1183,
			"versionNonce": 189707481,
			"isDeleted": false,
			"id": "Wu8J_U5msRrki6TJbY7CO",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1225.5603463188304,
			"y": -350.93842375820867,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 245.5603463188304,
			"height": 90.93842375820867,
			"seed": 263932759,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Cgb8RTJT",
				"focus": 0.49280594911076137,
				"gap": 14.439653681169602
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-245.5603463188304,
					90.93842375820867
				]
			]
		},
		{
			"type": "arrow",
			"version": 1008,
			"versionNonce": 1728582297,
			"isDeleted": false,
			"id": "Y41aZU_0yYHF9CBjWHqi-",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1324.5238294632659,
			"y": 59,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 44.52382946326588,
			"height": 219,
			"seed": 1851972439,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "BddSf0L1",
				"focus": 0.35244944666598393,
				"gap": 1
			},
			"endBinding": {
				"elementId": "6KEW8EUS",
				"focus": -0.8656510739712571,
				"gap": 30
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-44.52382946326588,
					-219
				]
			]
		},
		{
			"type": "arrow",
			"version": 520,
			"versionNonce": 852820057,
			"isDeleted": false,
			"id": "79pkQtWG_FvSc1iIik_Bc",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 774.6279069767443,
			"y": 19,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 5.372093023255729,
			"height": 179,
			"seed": 1174834873,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "gZfdMFC6",
				"focus": -0.11042337657663995,
				"gap": 1
			},
			"endBinding": {
				"elementId": "6KEW8EUS",
				"focus": 0.4719203614829535,
				"gap": 30
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					5.372093023255729,
					-179
				]
			]
		},
		{
			"type": "arrow",
			"version": 449,
			"versionNonce": 185695769,
			"isDeleted": false,
			"id": "HWf8qR0azRcDfyphU6ip8",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1009.7161038182135,
			"y": 38.84251581244172,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 10.283896181786531,
			"height": 198.84251581244172,
			"seed": 1039835415,
			"groupIds": [
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "UpbHhubK",
				"focus": -0.3726033426415296,
				"gap": 1.157484187558282
			},
			"endBinding": {
				"elementId": "6KEW8EUS",
				"focus": -0.21890058345404598,
				"gap": 30
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					10.283896181786531,
					-198.84251581244172
				]
			]
		},
		{
			"type": "text",
			"version": 136,
			"versionNonce": 1957804183,
			"isDeleted": false,
			"id": "gZfdMFC6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 720,
			"y": 20,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 122,
			"height": 25,
			"seed": 92167927,
			"groupIds": [
				"5w4yiqG_bix6UcF7dtU4s",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"79pkQtWG_FvSc1iIik_Bc"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "kernel-weight",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "kernel-weight"
		},
		{
			"type": "text",
			"version": 102,
			"versionNonce": 1126670137,
			"isDeleted": false,
			"id": "T0CMwoIF",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 720,
			"y": 40,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 164,
			"height": 23,
			"seed": 1668048985,
			"groupIds": [
				"5w4yiqG_bix6UcF7dtU4s",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "dimension: dx * dh",
			"baseline": 19,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "dimension: dx * dh"
		},
		{
			"type": "text",
			"version": 96,
			"versionNonce": 1005567703,
			"isDeleted": false,
			"id": "UpbHhubK",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 960,
			"y": 40,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 156,
			"height": 25,
			"seed": 1116418551,
			"groupIds": [
				"3bkC2wmpg3YeQ_CjLHf4T",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"HWf8qR0azRcDfyphU6ip8"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "recurrent-weight",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "recurrent-weight"
		},
		{
			"type": "text",
			"version": 114,
			"versionNonce": 1145969143,
			"isDeleted": false,
			"id": "DiROSqZt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 960,
			"y": 60,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 166,
			"height": 23,
			"seed": 1198629559,
			"groupIds": [
				"3bkC2wmpg3YeQ_CjLHf4T",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "dimension: dh * dh",
			"baseline": 19,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "dimension: dh * dh"
		},
		{
			"type": "text",
			"version": 91,
			"versionNonce": 250815063,
			"isDeleted": false,
			"id": "BddSf0L1",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1300,
			"y": 60,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 39,
			"height": 25,
			"seed": 1423058905,
			"groupIds": [
				"IirHE1ydbWmjq8DE9otuq",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Y41aZU_0yYHF9CBjWHqi-"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "bias",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "bias"
		},
		{
			"type": "text",
			"version": 102,
			"versionNonce": 821878233,
			"isDeleted": false,
			"id": "CdiAC1y5",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 1280,
			"y": 80,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 124,
			"height": 23,
			"seed": 254078295,
			"groupIds": [
				"IirHE1ydbWmjq8DE9otuq",
				"IUJmgICqVBr4HbAJwlXlW",
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "dimension: dh",
			"baseline": 19,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "dimension: dh"
		},
		{
			"type": "text",
			"version": 65,
			"versionNonce": 271271991,
			"isDeleted": false,
			"id": "wO9WNHfo",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 600,
			"y": -400,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 334,
			"height": 81,
			"seed": 373242713,
			"groupIds": [
				"9uaYKmVm7BE4cGs4CJLqx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 64,
			"fontFamily": 1,
			"text": "Simple RNN",
			"baseline": 57,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "Simple RNN"
		},
		{
			"type": "text",
			"version": 191,
			"versionNonce": 1941873593,
			"isDeleted": false,
			"id": "2XQBXydL",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "dashed",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 140,
			"y": 760,
			"strokeColor": "#a61e4d",
			"backgroundColor": "transparent",
			"width": 907,
			"height": 263,
			"seed": 460891927,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 36,
			"fontFamily": 3,
			"text": "encoder:\n    h<t> := f(h<t-1>, x<t>)\n    c    := h<T>\ndecoder:\n    h<t> := f'(h<t-1>, y<t>, c)\n    P(y<t>|y<1:t-1>) := g(h<t>, y<t-1>, c) ",
			"baseline": 254,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "encoder:\n    h<t> := f(h<t-1>, x<t>)\n    c    := h<T>\ndecoder:\n    h<t> := f'(h<t-1>, y<t>, c)\n    P(y<t>|y<1:t-1>) := g(h<t>, y<t-1>, c) "
		},
		{
			"type": "text",
			"version": 185,
			"versionNonce": 1517450713,
			"isDeleted": false,
			"id": "zBLkIRQf",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -1060,
			"y": 1280,
			"strokeColor": "#5f3dc4",
			"backgroundColor": "transparent",
			"width": 1121,
			"height": 104,
			"seed": 922430839,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 82.48561565017259,
			"fontFamily": 1,
			"text": "GRU (gated recurrent unit)",
			"baseline": 73,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "GRU (gated recurrent unit)"
		},
		{
			"type": "ellipse",
			"version": 92,
			"versionNonce": 1250600695,
			"isDeleted": false,
			"id": "ksu6YvXDNXZLbdIW_xabU",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -880,
			"y": 1420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#fa5252",
			"width": 40,
			"height": 40,
			"seed": 1760396217,
			"groupIds": [
				"veqyC8FHaySkL8fOS0eGx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"VG0jcpPrA1venLHJbksq6"
			]
		},
		{
			"type": "text",
			"version": 34,
			"versionNonce": 1980803065,
			"isDeleted": false,
			"id": "PNi1NO8v",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -820,
			"y": 1420,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 16,
			"height": 35,
			"seed": 1943446681,
			"groupIds": [
				"veqyC8FHaySkL8fOS0eGx"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "x",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "x"
		},
		{
			"type": "ellipse",
			"version": 92,
			"versionNonce": 1258417881,
			"isDeleted": false,
			"id": "cFe1HkvgW-C2dLhAAHOKD",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -880,
			"y": 1840,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 40,
			"height": 40,
			"seed": 157839447,
			"groupIds": [
				"iZ3bp41YAKcZKexb_xfoI"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"pAyEIhLyajn6p70BP3OKV"
			]
		},
		{
			"type": "text",
			"version": 39,
			"versionNonce": 1367066423,
			"isDeleted": false,
			"id": "BcUu9HIF",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -820,
			"y": 1840,
			"strokeColor": "#087f5b",
			"backgroundColor": "#4c6ef5",
			"width": 13,
			"height": 35,
			"seed": 1439059319,
			"groupIds": [
				"iZ3bp41YAKcZKexb_xfoI"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "y",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top",
			"rawText": "y"
		},
		{
			"type": "ellipse",
			"version": 131,
			"versionNonce": 411710103,
			"isDeleted": false,
			"id": "uJsGKjTJvFGPgfY9HSg8P",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -880,
			"y": 1620,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 178825593,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"ddsDWHT4fQ7WxlbOaT1Vz",
				"Bm4RFb3NYodnarIyv4k3V",
				"7mmieKGz7lH4cPAsSyeQF",
				"VG0jcpPrA1venLHJbksq6",
				"pAyEIhLyajn6p70BP3OKV",
				"s2GyoulFcqr1BuLQ3trfq",
				"0tUtKZ1FBtoovyVtm2voh",
				"fU7Cxo8lIQ_K38fTEmXZA",
				"bSiqf3DZ5OFC80iJld7h3"
			]
		},
		{
			"type": "arrow",
			"version": 164,
			"versionNonce": 402420505,
			"isDeleted": false,
			"id": "VG0jcpPrA1venLHJbksq6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -860,
			"y": 1461,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 0,
			"height": 158,
			"seed": 1353418391,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "ksu6YvXDNXZLbdIW_xabU",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "uJsGKjTJvFGPgfY9HSg8P",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					158
				]
			]
		},
		{
			"type": "arrow",
			"version": 163,
			"versionNonce": 143333431,
			"isDeleted": false,
			"id": "pAyEIhLyajn6p70BP3OKV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -860,
			"y": 1661,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 0,
			"height": 178,
			"seed": 111612505,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "uJsGKjTJvFGPgfY9HSg8P",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "cFe1HkvgW-C2dLhAAHOKD",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					0,
					178
				]
			]
		},
		{
			"type": "arrow",
			"version": 257,
			"versionNonce": 671956345,
			"isDeleted": false,
			"id": "fU7Cxo8lIQ_K38fTEmXZA",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -840,
			"y": 1620,
			"strokeColor": "#2b8a3e",
			"backgroundColor": "transparent",
			"width": 72,
			"height": 110.66668701171875,
			"seed": 1758239671,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "uJsGKjTJvFGPgfY9HSg8P",
				"focus": 0.23955573269050806,
				"gap": 8.284271247461902
			},
			"endBinding": {
				"focus": -0.4120319459119864,
				"gap": 12.509864705508583,
				"elementId": "uJsGKjTJvFGPgfY9HSg8P"
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					35.33331298828125,
					-50
				],
				[
					72,
					4
				],
				[
					70,
					49.3333740234375
				],
				[
					36,
					60.66668701171875
				],
				[
					8.66668701171875,
					35.3333740234375
				]
			]
		},
		{
			"type": "ellipse",
			"version": 144,
			"versionNonce": 1223559289,
			"isDeleted": false,
			"id": "Hvcwt3G9mKKPBAMIaeQRI",
			"fillStyle": "solid",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -540,
			"y": 1620,
			"strokeColor": "#087f5b",
			"backgroundColor": "#868e96",
			"width": 40,
			"height": 40,
			"seed": 995679895,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"p9tTA2MZr7ZcqyR8mlQdR",
				"4YDlBUsLgcANJknE1FnLW",
				"Q1fJNkOij8UEUTOYbUETz",
				"ddsDWHT4fQ7WxlbOaT1Vz",
				"Bm4RFb3NYodnarIyv4k3V",
				"7mmieKGz7lH4cPAsSyeQF",
				"VG0jcpPrA1venLHJbksq6",
				"pAyEIhLyajn6p70BP3OKV",
				"s2GyoulFcqr1BuLQ3trfq",
				"0tUtKZ1FBtoovyVtm2voh",
				"fU7Cxo8lIQ_K38fTEmXZA",
				"bSiqf3DZ5OFC80iJld7h3"
			]
		},
		{
			"id": "KJry_g1MGdOOwdFzNWO7E",
			"type": "diamond",
			"x": -440,
			"y": 1620,
			"width": 80,
			"height": 40,
			"angle": 0,
			"strokeColor": "#000000",
			"backgroundColor": "#82c91e",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"seed": 187668375,
			"version": 10,
			"versionNonce": 1290467639,
			"isDeleted": false,
			"boundElementIds": null
		}
	],
	"appState": {
		"theme": "light",
		"viewBackgroundColor": "#ffffff",
		"currentItemStrokeColor": "#000000",
		"currentItemBackgroundColor": "#82c91e",
		"currentItemFillStyle": "hachure",
		"currentItemStrokeWidth": 1,
		"currentItemStrokeStyle": "solid",
		"currentItemRoughness": 1,
		"currentItemOpacity": 100,
		"currentItemFontFamily": 3,
		"currentItemFontSize": 36,
		"currentItemTextAlign": "left",
		"currentItemStrokeSharpness": "sharp",
		"currentItemStartArrowhead": null,
		"currentItemEndArrowhead": "arrow",
		"currentItemLinearStrokeSharpness": "round",
		"gridSize": 20
	},
	"files": {}
}
```
%%
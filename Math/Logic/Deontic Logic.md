#logic  #law

# Alethic Modal Logic

**Alethic modal logic** is roughly the logic of _necessary truth_ and related notions.

Consider five basic alethic modal statuses, expressed as _sentential operators_--constructions that, when applied to a sentence, yield a sentence (as does "it is not the case that"):

```haskell
necessary :: Prop -> Prop
possible = neg . neccessary . neg
impossible = neccessary . neg
nonnecessary = neg . neccessary
contingent p = and (nonnecessary p) (possible p)
```

| sentential operator | natural language                              | probability    |
| ------------------- | --------------------------------------------- | -------------- |
| $\square$           | It is _necessary_ (necessarily true) that ... | $\Pr(p)=1$     |
| $\diamondsuit$      | It is _possible_ that ...                     | $\Pr(p) \ge 0$ |
| impossible          | It is _impossible_ that ...                   | $\Pr(p) = 0$   |
| non-necessary       | It is _non-necessary_ that ...                | $\Pr(p) \le 1$ |
| contingent          | It is _contingent_ that ...                   | $0\le \Pr(p)\le 1$               |

The following three rectangular cells are jointly exhaustive and mutually exclusive: every proposition is either _necessary_, _contingent_, or _impossible_, but no proposition is more than one of these.

```

+------------------------+
|        Possible        |
+-----------+------------+------------+
| Necessary | Contingent | Impossible |
+-----------+------------+------------+
            |      Non-Necessary      |
            +-------------------------+
```

>[!note]
>Recall that propositions are **contraries** if they can't both be true, **sub-contraries** if they can't both be false, and **contradictories** if they always have opposing truth-values.

![[Pasted image 20230204201728.png]]


# The Traditional Scheme of Deontic Logic

Normative Status

| Symbol        | natural language               |
| ------------- | ------------------------------ |
| $\mathbf{OB}$ | it is _obligatory_ that ...    |
| $\mathbf{PE}$ | it is _permissible_ that ...   |
| $\mathbf{IM}$ | it is _impermissible_ that ... |
| $\mathbf{GR}$ | it is _gratuitous_ that ...    |
| $\mathbf{OP}$ | it is _optional_ that ...      | 

>[!note]
> _gratuitous_ adj.
> 
> without good reason; unnecessary 
> 
> a gratuitous insult (无端的侮辱 )

```
+-----------------------+
|      Permissible      |
+------------+----------+---------------+
| Obligatory | Optional | Impermissible |
+------------+----------+---------------+
             |        Gratuituous       |
             +--------------------------+
```

```haskell
pe   = neg . ob . neg
im   = neg . pe
gr   = neg . ob
op p = and (neg $ ob p) (neg $ im p)
```

![[Pasted image 20230204205510.png]]

![[Pasted image 20230204205540.png]]

>[!warning]
>There is an obvious analog btw [[#Alethic Modal Logic]] and [[#The Traditional Scheme of Deontic Logic]], however, they are essentially different:
>
> 1. $\square\; p$ entails $p$
> 2. $p$ entails $\diamondsuit \; p$
> 
> but
> 1. $\mathbf{OB}\;p$ does not entail $p$.
> 2. $p$ does not entail $\mathbf{PE}\;p$


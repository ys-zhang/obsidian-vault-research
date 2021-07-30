
# Algorithm

***DP 沿着各个维度线性增长，每个状态上保留了以自身为目标的子问题的最优解***

# Problem

## LIS （Longest Increasing Subsequence)

```go
// input
var A []int     // input sequence
var n = len(A)

// ? 最长递增子序列长度

```

---
| state                                 | state transition function |
| ------------------------------------- | ------------------------- |
| $F[i]$ 表示以 $A[i]$ 结尾的子序列长度 |    $F[i+1]=1+\max_{A[j]\le A[i]} F[j]$                       |

---

## LCS (Longest Common Subsequence)
`char[] A, B`
求A, B 的最长公共子序列的长度

---

| state                                 | state transition function |
| ------------------------------------- | ------------------------- |
| $F_{ij}$ LCS of $A_{1:i}$,$B_{1:j}$  |    $F_{ij}=\max(F_{i-1,j-1}+\delta_{A_i,B_j}, F_{i-1,j}, F_{i,j-1})$                       |

---

## 数字三角形
给定 三角形 $A$， 求从 顶到底 路径最大和。 trivial

## Misc.

![[ex-mr-young-pic-perm.png]]

![[Pasted image 20210725170200.png]]

![[Pasted image 20210725175448.png]]

![[Pasted image 20210725175520.png]]
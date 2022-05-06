#statistics #hypothesis-test  


# Pearson's Chi-squared Test

>[!TLDR]
>Use the difference of expected frequency and observed frequency to test whether a random variable is from distribution $F$.


##### Hypothesis
- Null Hypothesis: $X \sim F$ 
- Alternative Hypothesis: $X \not \sim F$


##### Test Statistic

1. Partition range of $X$'s value in to $M$ _disjoint regions_.
2. Let $f_i$ be the number observed samples in range $i$,  $\sum_i f_i = N$  where the total number of samples is $N$.
3. Let $p_i$ be the theoretical probability of $X$ falls in to range $i$.
4. Test Statistic follows $\chi^2_{M-1}$:
$$
\chi^2 = \sum_{i=1}^M \frac{(f_i - Np_i)^2}{Np_i}
$$



# Chi-squared Test of Association

[Chi-Square Test of Independence - SPSS Tutorials - LibGuides at Kent State University](https://libguides.library.kent.edu/spss/chisquare)

>[!QUESTION]
The **chi-square test of independence** is used to analyze the **frequency table** (i.e. **contengency table**) formed by two _categorical variables_. The **chi-square test** evaluates whether there is a significant association between the categories of the two variables.


##### Hypothesis

-   **Null hypothesis ($H_0$)**: the row and the column variables of the contingency table are independent.
-   **Alternative hypothesis ($H_1$)**: row and column variables are dependent


##### Test Statistic

For each cell of the table, we have to calculate the _expected value $e$ under null hypothesis_:
$$
e_{ij} = \frac{\text{row}_i.\text{total} \times \text{col}_j.\text{total}}{\text{grand.total}}
$$
and
$$
\chi^2 = \sum_{ij} \frac{(o_{ij}-e_{ij})^2}{e_{ij}}
$$
The test statistic follows $\chi^2_{(R-1)\times(C-1)}$

#program-test #fuzzing 

coverage-based greybox fuzzing can be modeled as a Markov chain.

- A **state** $i$ is a specific **path** in the program.
- The **transition probability** $p_{ij}$ from state $i$ to state $j$ is given by the probability that fuzzing the seed which exercises path $i$ generates a seed which exercises path $j$.

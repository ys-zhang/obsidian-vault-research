#UML 

#  A Systematic Approach to Domain-Specific Language Design Using UML


In this paper, we first provide an overview of the new extensibility mechanisms of UML 2.1 and then describe a method for defining _profiles_ that greatly increases the likelihood of producing technically correct quality _UML profiles_.

A **stereotype** definition consisted of 
  - a user-defined stereotype name
  - a specification of the base UML concept (e.g., Class) for the stereotype
  -  optional constraints that specified how the base concept was specialized

Since stereotypes capture domain-specific concepts, they are typically used in conjunction with other stereotypes from the same domain. 
This led to the concept of a **profile**, a specially designated _UML package that contained a collection of related stereotypes_.

There are two significantly different ways in which **UML profiles** can be used:
1. A profile can be created to define a DSML (domain specific modelling language).
2. A profile can also be created to define a _domain-specific viewpoint_, 
# Introduction

## Raising the Level of Reuse

![[Pasted image 20220109161214.png]]

1. _Functions_: many function's output depends on knowledge of previous deductions; which result in sharing value among different functions, i.e., functions that are not pure.
2. _Objects_ suffers from changing of interfaces. Dividing work across vertical problem areas and defining interfaces between these areas is also problematic.

> Dividing work into horizontal subject-matter areas, or domain models, such as bank, database, authorization, user interface, and so forth, exposes interfaces at the level of rules.

# Concepts

## Model

- **Models** consist of sets of elements that describe some physical, abstract, or hypothetical reality.
- **Abstraction** involves ignoring information that is not of interest in a particular context;
-  **Classification** involves grouping important information based on common properties, even though the things under study are of course different from one another.
- A **metamodel** is simply _a model of a modelling language_. It defines the structure, semantics, and constraints for a family of models.
- A **platform** as the specification of an _execution environment_ for a set of models.
- A _platform_ has to have an implementation of the specification that the platform represents—in other words, at least one **realization** of it.
- A _realization_ that stands on its own is a **primitive realization**; a _realization_ comprised of one or more realizations is a **composed realization**.


```ad-note
Central to **MDA** is the notion of creating different _models_ at different levels of _abstraction_ and then linking them together to form an _implementation_.

A **metamodel** is merely a model whose instances are types in another model.
```

![[Pasted image 20220109163415.png]]

![[Pasted image 20220109202509.png]]

### Model Stack

- **M0** contains the data of the application (for example, the instances populating an object-oriented system at runtime, or rows in relational database tables). 
- **M1** contains the application: the classes of an object-oriented system, or the table definitions of a relational database. This is the level at which application modelling takes place (the type or model level). 
- **M2** contains the metadata that captures the modelling language: UML elements such as Class, Attribute, and Operation. This is the level at which tools operate (the metamodel or architectural level).
- **M3** is the _metametadata_ that describes the properties that metadata can exhibit. This is the level at which modelling languages and metamodels operate, providing for interchange between tools.

![[Pasted image 20220109203243.png]]

## Mapping

- A **mapping** between models is assumed to take one or more models as its input (these are the “sources”) and produce one output model (this is the “target”). 
- The rules for the mapping are described by **mapping rules** within a **mapping function**; the _mapping is an application of the mapping function_. These _rules are described at the metamodel level_ in such a way that they're applicable to all sets of source models that conform to the given metamodel.
- **Marks** are additional inputs of _mappings_ , which may associated with _mapping rules_.
- A _mark_ is defined by a **marking model**, which describes the _structure_ and _semantics_ of a set of types of _marks_.

![[Pasted image 20220109170501.png]]


## Other Concepts

- A **problem domain** is a _subject_ matter that can be understood _independently_ of other subject matters. Separately, each domain model is expressed in some kind of modelling language.

## Language

Any modelling language allows you to say some things but not others.

Each domain model is expressed in some kind of modelling language. 
- Mathematical equations are one language that we may use to describe air flow over a wing; 
- Symbols for resistors, capacitors, and various integrated circuits constitute a language used by electrical engineers;
- Scale drawings help civil engineers; 
- Hollywood gets by on recreational drugs.

By thinking about the problem at a high level of abstraction, specifying a system is much more convenient, efficient, and reusable. We increase productivity because we can say more, more quickly. The higher the level of the language, the greater the amount of functionality that can be delivered for a given amount of effort.
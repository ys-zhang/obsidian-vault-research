# Relational Model

data is organized into **relations** (called **tables** in SQL), where each relation is an **unordered collection** of **tuples** (**rows** in SQL)

## Relation

- One to One
- Many to One
- One to Many
- Many to Many

## Normalization

*rule of thumb*:  if you’re duplicating values that could be stored in just one place, the schema is not normalized.

> Anything that is meaningful to humans may need to change sometime in the future—and if that information is duplicated, all the redundant copies need to be updated. That incurs write overheads, and risks inconsistencies (where some copies of the information are updated but others aren’t). Removing such duplication is the key idea behind normalization in databases.ii

# Document Model

- Read and write/update as a whole.

## Cons

- Lack of support of join and normalization.


## Pros

- Schema flexibility
- Data locality for queries


# Graph Model

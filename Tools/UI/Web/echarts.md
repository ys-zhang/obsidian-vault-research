#visuallization

# Data Model


```
         +---- a column, when mapped to a series, 
         |     its has a "dimension name",
         |     and a "dimension type" 
         v 
+--------------------------+
|       | |                |
|       | |                | 
|       | |                |   <----- a table as `dataset.source`
|       | |                |
|       | |                | 
+--------------------------+
         v
         |
         |
         |  `series.encode` maps dimension to a series of data
         |
         |
         v
{
  x: [...................],
  y: [...................]
}


```
1. _source_: source 
1. _dimension_: 

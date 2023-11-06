#data-structure #compiler 
#program-synthesis 

- An _e-graph_ is a data structure to maintain a _congruence relation_ over _expressions_. 
- An _e-graph_ is a set of _e-classes_(equivalent classes), each of which consist of _e-nodes_
- An _e-node_ is an operator with children (which are _e-classes_) 

```haskell

newtype EGraph = 
  MkEGraph { unwrapEGraph :: [EClass] }

newtype EClass = 
  MkEClass { unwrapEClass :: [ENode] }

type ENode op = MkENode { 
      op :: op 
      children :: [EClass]
    }
    
type Language = ENode
```
We say a _term_ is _represented by_ or _contained in_
- a _e-graph_ iff. one of the _e-graph_'s _e-class_ do  
- a _e-class_ iff. one of its _e-node_ do 
- a _e-node_ iff the term has the form `f(t1, t2, ...)` and the node has the form `f(n1, n2, ...)` and `ni` represents `ti`


# Example 

```haskell 
import Data.Tree
data Node = MkNode {
    payload :: Either String Int
  } 
```

# References 


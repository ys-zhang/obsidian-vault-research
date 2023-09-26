#algorithm #graph


# Z-Graph (Control Flow Graph zipper)

A **Control-flow graph** is a collection of nodes, each represents a single instruction.

We have 3 different types of nodes:

1. _first_: a node that could be the **target** of a _back edge_.
2. _middle_: a node that is reached only its immediate predecessor and reached only its immediate successor.
3. _last_: a node that could be the _source_ of a _back edge_.

Nodes in CGF are organised into **basic blocks**, which begins with a _first node_ followed by 0 or more _middle nodes_ and ends with a _last node_.

- the _first node_ must be a **label** (or a the entry node of the program).
- the _last node_ must be a **control transfer instruction** (or the exit node). For instance, `jmp`, `ret`


```haskell
type Uid = Int
type Label = (Uid, String)  -- ^ (unique id, asm-code name) 
```

> A _zipper graph_, or _z-graph_ for short, is a graph with the focus on one particular **edge**.

```haskell
data Graph
data ZGraph

-- | Given a graph, we can on various places, and lose focus
entry :: Graph -> ZGraph
exit  :: Graph -> ZGraph
focus :: Uid -> Graph -> ZGraph
unfocus :: ZGraph -> Graph
```

## Construct a graph

Each of the following constructors takes a z-graph, inserts one or more nodes at the focus, and leaves the focus just before the newly inserted nodes.

```haskell
-- | an empty graph with entry and exit as the same node
empty :: Graph   
-- | Node can be seen as insert actions
type Nodes = ZGraph -> ZGraph
data Machine -- | IR is machine dependent
label :: Machine -> Label -> Nodes
instruction :: IR -> Nodes
branch :: Machine 
       -> Label
       -> Nodes
cbranch :: Machine 
        -> Expr  -- ^ text expr
        -> Label -- ^ if true jmp to
        -> Lable -- ^ if false jmp to
        -> Nodes
call :: Machine 
     ->  ...
ret :: IR -> Reg -> Nodes
```

## Implementation

```haskell
data First = Entry | Label Label 
newtype Middle = Instruction IR
data Last = Exit | Jmp Label

type Block = (First, [Middle], Last)
data ZBlock = First First [Middle] Last
            | Middle First [Middle] {- reverse order-} Middle [Middle] Last
            | Last First [Middle] {- reverse order -} Last

type Graph = Map Label Block 
type ZGraph = (ZBlock, Map Label Block) -- ^ (focus block, other blocks)
```


# Zippo (Zipper by lens)

the `zippo` [lib](http://brandon.si/code/zippo/) uses lens for moving focus.

```haskell
data Zipper st a = Zipper 
  { stack :: st a
    -- ^ a history of focus, i.e. crumbs 
  , view :: a }

-- Top :> Tree a ~ (:>) Top (Tree a) 
data (:>) st a c = Snoc (st a) (c -> a)

(Top :> Tree a) :: * -> *
Top :> Tree a   ~  (:>) Top (Tree a)

Snoc Top :: (c -> Tree a) -> Top :> (Tree a) c 
```

A zipper is something like a chain with a pointer, for instance the zipper
`A->B->C->D` focuses on `D` which lives on the 3rd level of a tree rooted at `A`:
$$
D \cup (C-D) \cup (B-C) \cup (A-B)
$$
to move the focus up, the `moveUp` function must recover the information of its upper level which is a function 
$$
  D\cup (C-D) \xrightarrow{\text{moveUp}} C 
$$

```
*---------*---------------------------*
|    D    | moveUp :: D -> (C-D) -> C |
*---------*---------------------------*
|   C-D   | moveUp :: C -> (B-C) -> B | 
*---------*---------------------------*
|   B-C   | moveUp :: B -> (A-B) -> A | 
*---------*---------------------------*
|   A-B   | moveUp :: A -> TOP -> TOP | 
*---------*---------------------------*
|   TOP   | moveUp :: _ -> TOP -> TOP | 
*---------*---------------------------*
```




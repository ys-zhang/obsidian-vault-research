![rust-analyzer impl](https://www.youtube.com/watch?v=n5LDjWIAByM&list=PLhb66M_x9UmrqXhQuIpWC5VgTdrGxMx3y&index=8)


# Requirements

1. full-fidelity: white space and comments are part of the tree
2. resilient and semi-structured: (can represent arbitrary invalid code)
3. value typed
4. immutable
5. cheaply updatable: cheap refactoring
6. conveniently updatable
7. easy to navigate: accessing parent, children, siblings

# Ideas

## "dynamically typed tree"

```rust 
struct SyntaxKind(u16);
const IDENT = SyntaxKind(0);
// ..

enum NodeOrToken {
  Node(Node),
  Token(Token)
}

struct Token {
  kind: SyntaxKind,
  text: String,
}

struct Node {
  kind: SyntaxKind,
  children: Vec<NodeOrToken>,
  len: usize,  // total text len of the node, all children included
}

impl Node {
  // cheap update
  fn replace_child(&self, idx: usize, new_child: NodeOrToken) -> Self {
    ...
  }
}
```

The problem of this implementation is when try implementing the `replace_child` method, the whole subtree will be deep cloned which is inefficient. 

## avoid deep copy by pointers

```rust 
type Token = Arc<TokenData>;

struct TokenData {
  kind: SyntaxKind,
  text: String,
} 

type Node = Arc<NodeData>;

struct NodeData {
  kind: SyntaxKind,
  children: Vec<NodeOrToken>,
  len: usize,  // total text len of the node, all children included
}
```

For now we have the **green tree**; however, the problem is that the tree is difficult to _navigate_(how to access `parent`)

## Red Node

Goals:
1. access node `parent`
2. get node pos in the buffer, `.text_range`

```rust
type RedNode = Rc<RedNodeData>

struct RedNodeData {
  green: Node,
  parent: RedNode,
  text_offset: usize, // offset in buffer
}
```

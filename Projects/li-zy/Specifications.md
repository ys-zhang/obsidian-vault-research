#project 
#Haskell


# AST 

1. An `ASTNode` can have **properties**
2. An `ASTNode` can have **children**
3. An `ASTNode` can have **references**, which also including nodes with the same type
4. An `ASTNode` can request access of its _parent_ and _siblings_ (of cause its _props_, _children_ and _refs_ as it owns them)
5. An `ASTNode` can get its position in its parent
6. An `ASTNode` can find all other nodes that **refers** to it.
7. other aspects such as editor, type checking, validator can also access the AST.
8. need to sport a trie structure for completion menu generation


## TODO

### 1. Write a macro for language implementation 

```rust
/// The Petro Language
#[derive(Clone)]
pub enum Petro {
    Well(NodeWrapper<Well>),
    WellParameter(NodeWrapper<WellParameter>),
    WellParameterDefinition(NodeWrapper<WellParameterDefinition>),
    WellParameterEnumValueEntry(NodeWrapper<WellParameterEnumValueEntry>),
    WellParameterValue(NodeWrapper<WellParameterValue>),
}

impl Language for Petro {
    fn id() -> &'static str {
        "base.petro"
    }
    fn name() -> &'static str {
        "petro"
    }
}

impl From<NodeWrapper<Well>> for Petro {
    fn from(wrap: NodeWrapper<Well>) -> Self {
       Petro::Well(wrap) 
    }
}

impl From<NodeWrapper<WellParameter>> for Petro {
    fn from(wrap: NodeWrapper<WellParameter>) -> Self {
        Petro::WellParameter(wrap)
    } 
}

impl From<NodeWrapper<WellParameterDefinition>> for Petro {
    fn from(wrap: NodeWrapper<WellParameterDefinition>) -> Self {
        Petro::WellParameterDefinition(wrap)
    } 
}

impl From<NodeWrapper<WellParameterEnumValueEntry>> for Petro {
    fn from(wrap: NodeWrapper<WellParameterEnumValueEntry>) -> Self {
        Petro::WellParameterEnumValueEntry(wrap)
    } 
}

```

# Editor

Editor is responsible for the UI of the [[#AST]].
1. Information from other aspect should be able to be shown on the editor
2. there is no 1-1 correspondence between an editor to an AST node:
    - a node concept or ASTNode Type can have multiple editors
    - a node can be shown on different places of the app window
3. there should be a mechanism to tricker actions defined by other aspects:
    - show docs of a node
    - build, run, debug the model represented by the AST
4. the editor can use different types of _projection_.
    - text editor like, table like, diagram, latex like
    - interactive component 


```haskell
editor :: GlobalEditorSettings -> ASTNode -> LocalEditorSettings -> (UI, UserInput -> ASTNode)  
```

For each _editor instance_ shown on the app window 

``` haskell 

class ASTNode a where
  ..

-- Process user input to
-- Monad state represents the internal state of the editor
class (ASTNode a, Monad state) => Editor state a where
  proc :: state a ->  UserInput -> state a
  show :: GlobalConfig -> state a -> LocalConfig -> UI    -- this is where yew steps in


data EditorState = EditorState {
    hasFocus :: Bool;  -- jump through nodes
    nextFocus :: Editor
  }
```



# Live Model Checking

## Scoping
## Property Constrains


# DSL

a _contract_ basically describes at each time $t$ on what _condition_ some _event_ must (not) / may happen. This has a connection with [[Deontic Logic]].

```haskell
obligations :: (Time t, Event e, State s) => Contract -> s -> t -> [e] 
```

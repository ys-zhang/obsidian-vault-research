#DSL 
#MPS



# Scope

[scope reference](https://www.jetbrains.com/help/mps/scopes.html#inheritedscopes)

- There are 2 ways to define scopes
    1. _inherited scope_, defined by implement _ScopeProvider_ interface
    2. _reference scope_, defined in _Constrains_ of the concept

## Inherited Scope

```
+-------------------------------------------+
| Closest Ancestor Implements ScopeProvider |----+
+-------------------------------------------+    |
                      |                          |
                     ...                         |
                      |                          |
                      v                          |
            +--------------------+    provides   |
            | Need for concept A |<--------------+
            +--------------------+     scope

```

A `ScopeProvider` implements the `getScope(concept<> kind, node<> child)` method where
- `kind` is the concept of the possible targets for the reference
- `child` is the _child_ node of this ScopeProvider, from which the request case, so the actual reference is among descendants of the `child` node. 


# Editor

The main purpose of _editors_ is to facilitate editing AST: 

1. add new nodes to AST
2. remove nodes from AST
3. replace nodes in the AST



![[MPS-Editor.excalidraw|100%]]

There are three tools provided by _editor cells_ can help us do the job:
1. key maps
2. action maps
3. cell menu (a custom completion menu), which including the following parts/sections:


```rust
struct KeymapItem = {
  keyStrokes: Set<Key>,
  // determines if a key map item is applicable here
  applicable: Fn() -> bool,
  // caret policy says where in a cell a caret should
  // be located to make this key map item enabled.  
  caretPolicy: CaretPolicy,
  action: &Action
}

enum CaretPolicy {
  AnyPos,  // default
  FirstPos, LastPos, IntermediatePos, 
}

struct Action {
  description: &str,
  execute: Fn(&EditorContext, &mut Node) -> ()
}

enum DefaultAction {
  Delete(&Action), Backspace(&Action),
  SelectAll(&Action)
}
```


## Cell Menu (Completion Menu)

```
    MENU   
+-----------+
|menu part 1|   MENU PARTs are 
+-----------+     SECTIONs in the MENU,
|menu part 2|     containing MENU ITEMs.
+-----------+
|    ...    |
+-----------+
|menu part N|
+-----------+

```


> Editor > Common > menu

$$
\text{Menu Parts} = 
\begin{cases}
  &\text{Property Value Menu Part} \\
  &\text{Property Postfix Hints Menu Part} \\
  \\
  &\text{Primary Replace Child Menu} \\
  &\text{Replace Child Menu} \\
  \\
  &\text{Primary Choose Referent Menu} \\
  \\
  &\text{Replace Node Menu} \\
  \\
  &\text{Generic Menu Item Part} \\
  &\text{Action Group Part}
\end{cases}
$$


```
        CELL(COMPLETION) MENU ITEM
+----+-------------+-------+-----------+
|Icon|Matching Text|       |Description|
+----+-------------+-------+-----------+
```

For each item in any menu parts, the following fields needs to be specified:
1. icon
2. matching text
3. description
4. action
and some optional parameters
  - parameter object
  - output concept

_Different menu parts provide different "template" and "default settings" of these fields:_

| Menu parts                      | Action & Misc.                          |
| ------------------------------- | --------------------------------------- |
| property value                  | replace property value                  |
| property postfix                | replace property value by add a postfix |
| primary xxxx                    | default menu parts                      |
| replace node/child menu         | replace node/child with new nodes       |
| generic menu                    | arbitrary action can be associated      |
| generic action group            | create menu item dynamically            |
| replace child/node action group |                                         |


## Transformation Menu

Transformation menu language is used to define _transformation menus_ which is a hierarchical structure of _submenus_ and _actions_ that can appear in various locations in the _editor_, including:
- _side transformation menu_
- _substitute menu_
- _context assistant_
- _context action tool_

![[MPS-editor-menus.excalidraw|100%]]

### Transformation Menu & Substitute Menu

A concept can be associated with a _transformation menu_ and a _substitution menu_. the idea is 
- a node of Concept $A$ can be _transformed_ into a node of concept $B$.
- or equivalently, a node of concept $B$ can be used to _substitute_ a node of concept _A_.
thus 
- _transformation menu_ is associated with concept $A$.
- and _substitute menu_ is associated with concept $B$.

```
+-----------------+   +-------------------+
|node of concept A|   |transformation menu|<---+
+--------+--------+   +-------------------+    |
         |                                     |
         | BECOME                 INCLUDED IN  |
         |                                     |
         v                                     |
+-----------------+   +--------------+         |
|node of concept B|   |substitue menu|---------+
+-----------------+   +--------------+      
```

>[!NOTE] Default Menu
>Each concept has a default transformation menu associated. If no transformation menu is provided explicitly, the one for the closest superconcept is used.

>[!NOTE] Menu Contribution
> Extends a given menu by contributing additional menu parts to an existing menu.
>
> Typical usage is when extending a language and we need to adding new menu items to some menu defined in the extended language.


### Algorithms

>[!Algorithm] Populate Completion Menu
> 1. If your selection is inside of a position which allows concept $A$, then all _enabled subconcepts_ of $A$ will be available in the _completion menu_.
> 2. All abstract concepts are excluded.
> 3. All concepts, for which the _'can be a child'_ constraint returns _false_, are excluded
> 4. All concepts, for which the _'can be a parent'_ constraint of a parent node returns _false_, are excluded.
> 5. If a concept contains a _1:1 reference_, then it is not added to the completion menu itself. Instead, an item is added for each element in scope for that reference. We use the name **smart reference** for such items.


>[!NOTE] Debug menu
> `<Ctrl>/<Cmd> + <Alt> + B` can track which menu provides the selected menu item



# Generator

 
$$
\text{Generator Spec} = \begin{cases}
  &\text{generator model } 1 \\
  &\vdots \\
  & \text{utility model } 1 \\
  & \vdots   
\end{cases}
$$
![[MPS-generator.excalidraw|center]]  


A **generator model** consists:
- _templates_, including inherited templates (from _dependent generators_)
- _mapping constrains_, which are collection of generator rules
```haskell
-- s, d are src and dst language ast 
-- ctx is the genContext
class GeneratorRule a where
  -- specified using
  -- 1. concept filter
  -- 2. condition function
  predicate :: a -> ctx -> s -> Bool
  -- specified using inline/external template
  consequence :: a -> s -> d  

class GeneratorContext c where
  newUniqueName :: c -> String
  findNode :: c -> Path -> d
```

![[Pasted image 20221021000810.png]]

## Macros

Recall a _Concept_ contains _properties_, _children_ and _references_, all of these need to be constructed to make a concept install or AST node of the destination language.
the tools to create these are
- _property macro_
- _reference macro_
- _node macro_
![[Pasted image 20221021120924.png]]

>[!KEYMAP] 
> Code wrapping (that is the creation of a new macro) is done by pressing _Ctrl+Shift+M_ or by applying the 'Create macro' intention.

## Generation Stages

1. Specify all _generator models_ involved
2. Specify priority of _transformations_
3. run all _transformation steps_
4. generate text files
5. compile


# Misc.

1. _constrains_ seems to be a global fence for all settings including actions, substitution, etc. For instance, a substitute menu with some action transform $A \to B$ in an AST (or model), the action will not run if $B$ violates the constrains at the actual position in the AST, even though it can pass the `can execute` predicate defined in the action of the substitute menu.
2. The _DELETE_ and _BACKSPACE action_ handlers have specific semantics. If an action map **defines or imports** a DELETE handler and **does not define or import** a BACKSPACE handler, a default BACKSPACE handler identical to the one for DELETE is registered automatically.
3. (_wrap substitute menu_) you can create a _wrap substitute menu part_ in _substitution menu_ for some concept $A$.  The menu part wraps a concept $B$, which means, the substitution menu item's associated action will substitute the node with a new instance of concept $A$ which wraps a node of concept $B$
4. _someNode.replace with(..)_ will throw an exception if `someNode` is null



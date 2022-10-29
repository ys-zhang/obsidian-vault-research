#DSL 

# Generator

$$
\text{Generator Spec} = \begin{cases}
  &\text{generator model } 1 \\
  &\vdots \\
  & \text{utility model } 1 \\
  & \vdots   
\end{cases}
$$
![[MPS-generator.excalidraw | 100%]]


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

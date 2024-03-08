#program-analysis #compiler 


>[!def] collecting semantics
>We call a version of the program semantics that has been augmented with additional information necessary for some particular analysis a collecting semantics.

```haskell
data ProgramState a = ProgramState { lookup :: Var -> (Value, a) } 

type Semantics a 
  =  Instr -- instruction
  -> (ProgramState a, ProgramCounter) -- augmented program configuration
  -> (ProgramState a, ProgramCounter) -- augmented program configuration
```

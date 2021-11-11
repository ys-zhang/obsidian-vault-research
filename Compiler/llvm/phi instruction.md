#llvm 

> The ‘`phi`’ instruction is used to implement the $\phi$ node in the SSA graph representing the function.

> At runtime, the ‘`phi`’ instruction logically takes on the value specified by the pair corresponding to the predecessor basic block that executed just prior to the current block.

 $\phi$ node is a ternary expression. One might argue that it doesn't contain the condition, but really, upon converting to the final code, you can't determine otherwise which one of arguments is live, so φ have to have the condition too.
 
    <result> = phi [fast-math-flags] <ty> [ <val0>, <label0>], ...
 
 
```c
void m(bool r, bool y){
    bool l = y || r ;
}
```

```
define void @_Z1mbb(i1 zeroext %r, i1 zeroext %y) nounwind {
entry:
  %r.addr = alloca i8, align 1
  %y.addr = alloca i8, align 1
  %l = alloca i8, align 1
  %frombool = zext i1 %r to i8
  store i8 %frombool, i8* %r.addr, align 1
  %frombool1 = zext i1 %y to i8
  store i8 %frombool1, i8* %y.addr, align 1
  %0 = load i8* %y.addr, align 1
  %tobool = trunc i8 %0 to i1
  br i1 %tobool, label %lor.end, label %lor.rhs

lor.rhs:                           ; preds = %entry
  %1 = load i8* %r.addr, align 1
  %tobool2 = trunc i8 %1 to i1
  br label %lor.end

lor.end:                           ; preds = %lor.rhs, %entry
  ; equals true if jmp from block %entry
  ; equals %tobool2 if jmp from block %lor.rhs
  %2 = phi i1 [ true, %entry ], [ %tobool2, %lor.rhs ]
  %frombool3 = zext i1 %2 to i8
  store i8 %frombool3, i8* %l, align 1
  ret void
}
```

# Reference
[LLVM Language Reference Manual — LLVM 13 documentation](https://llvm.org/docs/LangRef.html#phi-instruction)

[What exactly PHI instruction does and how to use it in LLVM - Stack Overflow](https://stackoverflow.com/questions/11485531/what-exactly-phi-instruction-does-and-how-to-use-it-in-llvm#:~:text=The%20%27phi%27%20instruction%20is%20used%20to%20implement%20the,still%20hard%20to%20understand%20what%20it%20does%20exactly.)



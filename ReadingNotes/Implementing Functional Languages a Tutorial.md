#compiler #functional-programming 

# Chap 1: the Core language

## Constructor

>[!question] 
> How to represent and manipulate structured types?
> - How to represent data constructors?
> - How to do pattern matching?

```haskell
data Constr = 
  MkConstr 
    { tag :: Word     -- identifies the constructor
    , arity :: Word } -- the arity of the constructor
```

In a _well-typed_ program, objects of different type will never need to be distinguished at run-time, so _tags_ only need to be unique within a data type.

## Expression

>[!definition] binder
> A _binder_ is the name used at the _binding occurrence_ of a variable; that is, on the left-hand side of a `let(rec)` definition, or in a _lambda abstraction_.


>[!def] supercombinator
> [supercombinators](https://en.wikipedia.org/wiki/Supercombinator) are mathematical expressions that are _fully bound_ (no free variable) and _self contained_.
> 
> It can be either  
> - a constant, or
> - a combinator where all the sub-expressions are _supercombinators_

>[!def] constant applicative form (CAF)
> _supercombinators_ with no arguments

```haskell
data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int        -- constructor
  | EAp (Expr a) (Expr a)  -- application
  | ELet 
      IsRec                -- boolean: rec or not
      [(a, Expr a)]        -- bindings
      (Expr a)             -- body
  | ECase
      (Expr a)             -- expr to scrutinise
      [Alter a]            -- cases
  | ELam [a] (Expr a)
  deriving (Show)
  
type IsRec = Bool
type Alter a = 
  ( Int   -- tag of the constructor
  , [a] 
  , Expr a)

-- | supper combinator definition
type ScDefn a = (Name, [a], Expr a)

type Program a = [ScDefn a]
```


# Chapter 2 Template instantiation

> [!tldr]
> - A functional program is ‘executed’ by evaluating an expression.
> - The expression is represented by a graph.
> - Evaluation takes place by carrying out a sequence of reductions.
> - A reduction replaces (or updates) a reducible expression in the graph by its reduced form. The term ‘reducible expression’ is often abbreviated to ‘redex’.
> - Evaluation is complete when there are no more redexes; we say that the expression is in normal form.
> - (_reduce strategy_) At any time there may be more than one redex in the expression being evaluated, so there is a choice about which one to reduce next. Fortunately, whatever reduction sequence we choose, we will always get the same answer (that is, normal form). There is one caveat: some reduction sequences may fail to terminate.
> - However, if any choice of redexes makes evaluation terminate, then the policy of always selecting the outermost redex will also do so. This choice of reduction order is called normal order reduction, and it is the one we will always use.

>[!def] redex
> _"redex"_ is short for _reducible expression_, which means an expression not in its normal form and can be further reduced by some reduction. 


>[!algorithm] evaluation algorithm
> ```
> until there are no more redexes 
>   select the outermost-leftmost redex 
>   reduce it 
>   update the (root of the) redex with the result 
> end
> ```

>[!note] judging redex
> The strategy of deciding whether an expression is reducible
> is critical in the evaluation algorithm.
> 
> In general, you can add preconditions to reduction rules when testing whether it is eligible to an expression. 
>
> To be more specific, these condition needs to be considered
> 1. Are supercombinators redexes?
> 2. Are built-in primitives redexes?
>
> Usually the answer to question 1 is Yes; and, built-in primitive applications are reducible iff any of the arguments are reducible.
>
> The reason of the difference is that these built-in primitives shall be evaluated at runtime; sometimes the compiler even do not know how to evaluate them since we may targets a different platform.
> It’s the same case for symbols in dynamic loaded modules and FFI.

## Unwinding the spine to ﬁnd the next redex

It is easy to ﬁnd the outermost function application (though it may not be reducible) as follows:
1. Follow the left branch of the application nodes, starting at the root, until you get to a supercombinator or built-in primitive. This left-branching chain of application nodes is called the spine of the expression, and this process is called unwinding the spine. Typically a stack is used to remember the addresses of the nodes encountered on the way down.
2. Now, check how many arguments the supercombinator or primitive takes and go back up that number of application nodes; you have now found the root of the outermost function application.

```haskell
-- | the outermost reducible function application
withSpine :: (Expr a -> Expr a) 
          -- ^ func deals with the outermost redex
          -> Expr a  
          -- ^ root of the expr
          -> Expr a  
          -- ^ updated expr
withSpine (EAp f a) = _
withSpine _ = id
```

>[!def] weak head normal form (WHNF)
>An expression is in weak head normal form (WHNF), if it is either:
> - a constructor (eventually applied to arguments) like `True`, `Just (square 42)` or `(:) 1`.
> - a built-in function applied to too few arguments (perhaps none) like `(+) 2` or `sqrt`.
> - or a lambda abstraction `\x -> expression`.
>
> **It does not require the body of lambda or arguments of application in WHNF.**
> 
> _Note that the arguments do not themselves have to be fully evaluated for an expression to be in weak head normal form._

>[!tldr]
> The intuition behind the concept of WHNF is that it is the exact form in the sense that if we what to reduce `f a` then we have to first reduce `f` to WHNF. 
> 
> In other words, 
> - if `f` is not in WHNF, then we cannot apply reduce rules to the application expression, 
> - if we do more reduction further to `f`, then some portion of the work may be wasted due to laziness.

## State transition system

>[!def] state transition system
>A _state transition system_ is a notation for describing the behaviour of a _sequential machine_. At any time, the machine is in some _state_, beginning with a speciﬁed _initial state_. 
>
>- If the machine’s state matches one of the _state transition rules_, the rule _ﬁres_ and speciﬁes a new state for the machine;
>- If no rules matches, execution _halts_;
>- If more then one rule matches, then one is chosen _arbitrarily_ to fire; the machine is then _non-deterministic_.

# G-Machine

>[!def] supercombinator
> a _supercombinator_ `$S` (the `$` is a dedicated name prefix for all supercombinators) with arity $n$ is a _lambda expression_  of the form 
> $$
> \lambda x_1 \;.\; x_2\; \dots \; x_n \; . \;E
> $$
> such that 
> 1. `$S` has no free variables, thus we do not need a global context for global variable 
> 2. any lambda expression in $E$ is a supercombinator, which means they do not have free variables and thus we do not need to keep a local context for local variables
> 3. $E$ is **not** a lambda expression
> 4. $n\ge 0$
>
> and,
> - A _supercombinator redex_ consists of the _application_ of an _arity $n$ supercombinator_ to $n$ arguments. 
> - A _supercombinator reduction_ replaces a _supercombinator redex_ by an _instance of its body_ with arguments substituted for free occurrences of the corresponding formal parameters.

Since they have no free variables (clause (1)) we can compile a fixed code sequence for them. Furthermore, clause (2) ensures that any lambda abstractions in the body have no free variables, and hence do not need to be copied when instantiating the supercombinator body.


>[!example] supercombinator
> the follow are **not** supercombinators
>$$
> \begin{align}
> \lambda x &. y   & \text{y occurs free}\\
> \lambda f &. f \; (\lambda x . f\; x \; 2) & \text{inner lambda is not supercombinator}
> \end{align}
>$$

>[!def] combinator
> A _combinator_ is a lambda expression which contains no occurrences of a free variable

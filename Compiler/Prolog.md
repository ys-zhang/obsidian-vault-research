# Basics

[swi-prolog doc](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
[build-in predicates](https://www.swi-prolog.org/pldoc/man?section=builtin)

## Syntax

### Term

```haskell
{- | Atom, Number and Variable are called simple terms.
Atom and Number are called constants.
Structure is the only Complex term -}
data Term 
  = SimpleTerm SimpleTerm
  | ComplexTerm Structure

data SimpleTerm
  = Atom Atom
  | Number Integer
  | Variable String
  
newtype Atom = MkAtom String
data Structure = MkStructure 
  { functor :: Atom
  , args :: [Term] 
  }

arity :: Term -> Int
arity (SimpleTerm _)  = 0
arity (ComplexTerm s) = length $ args s
```

### Clause

[Stack Overflow: Fact, Rule, Procedure, Predicate](https://stackoverflow.com/questions/49898738/is-this-prolog-terminology-correct-fact-rule-procedure-predicate)

```haskell
import Prelude hiding (head)

data Clause 
  = Fact { head :: CallTerm } 
  | Rule { head :: CallTerm, body :: [Goal]}
  
type CallTerm = Either Atom Structure
type Goal = CallTerm
```


Some Concepts:
- **ground term**:  _term_ without unassociated _variables_.
- A **directive**  (`:- <term>`) is an instruction to the compiler:
    - set (predicate) properties (see [declare](https://www.swi-prolog.org/pldoc/man?section=declare)), 
    - set flags (see [set_prolog_flag/2](https://www.swi-prolog.org/pldoc/man?predicate=set_prolog_flag/2)) 
    - load files (see [consulting](https://www.swi-prolog.org/pldoc/man?section=consulting)). 
- The term **evaluating a goal** is used to mean determining whether or not it is satisﬁed.     


## Byrd Box Model

See also [debugger ref](https://www.swi-prolog.org/pldoc/man?section=debugoverview), [[#Debug Predicates]]
```
            *--------------------------------------*
     Call   |                                      |   Exit
 ---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
            |                                      |
            |  descendant(X,Z) :-                  |
 <--------- +     offspring(X,Y), descendant(Y,Z). + <---------
     Fail   |                                      |   Redo
            *--------------------------------------*
```

 ports:
 
 - standard ports  `call`, `redo`, `exit`, `fail`
 - extra ports:. `unify`, `exception`


```
      push rule body goals on DFS stack
    +-----------------------------------+
    |                                   |
    v                                   | move to rule body
+------+               +------+         |
| call |   rule head   | unify|---------+
+------+-------------->+------+
| redo |    matches    | fail |
+------+               +------+
```

To prove a predicate $f/N$.

1. match all _rules_ which together defines `f/N`.
    - match succeeds make the `call` port is triggered
    - if the match happens in a **backtrace** `redo` is triggered
2. try to _unify_ the matched rule with the predicate
    - `unify` is traced with the results of unification if one is found.
    - `fail` is traced if no rule heads can be unified.
3. can't find more ungrounded predicates
    - `exit` if the rule is _true_.
    - `fail` if the rule is _false_.
    - `exception` if exception is thrown.


- DETERMINISTIC PREDICATE (well-behaved: closing off the REDO port, i.e. "leaving no choicepoint")
```
             +---------------------+
     -------⊳|Call------⊳-----⊳Succ|------⊳
   from      |                     |      to
   prev      |                     |      next
   pred      |                     |      pred
     ⊲---+   |Fail             Redo|   +---
         |   +---------------------+   |
         |                             |
         +---⊲----no choicepoint----⊲--+
```
- SEMI-DETERMINISTIC PREDICATE (well-behaved: closing off the REDO port, i.e. "leaving no choicepoint")
```
              
             +---------------------+
     -------⊳|Call---+--⊳-----⊳Succ|------⊳
   from      |       |             |      to
   prev      |       ∇             |      next
   pred      |       |             |      pred
     ⊲---+---|Fail⊲--+             |   +---
         |   +---------------------+   |
         |                             |
         +---⊲----no choicepoint----⊲--+
```
- others (well-behaved if it closes off the REDO port at the last solution, i.e. "leaves no choicepoint")
  - NONDETERMINISTIC PREDICATE (succeeds maybe 0 times)
  - MULTI PREDICATE (succeeds at least once)
```
              
             +---------------------+
     -------⊳|Call---+--⊳--+--⊳Succ|------⊳
   from      |       |     |       |      to
   prev      |       ∇     ∆       |      next
   pred      |       |     |       |      pred
     ⊲---+---|Fail⊲--+--⊲--+---Redo|⊲--+---
         |   +---------------------+   |
         |                             |
         +---⊲----no choicepoint----⊲--+
```


Concepts:
- _Trace mode_ is the main Prolog command line _debugger_ that allows for _tracing the transitions_ of ports
- _Trace points_ are a separate feature from _trace mode_ that allow writing specified ports to the console when a predicate is being evaluated.


## Module and Compilation

- `load.pl` is a file contains only `use_module/[1,2]`,  `ensure_loaded/1` and possibly, a **entry point**.
- In **script mode** `initialization/[1,2]` can call goal after loading the source file.
- **Saved state** mode supports run without `swipl` dev env installed. 
    - `qsave_program/[1,2]` or `swipl [opt-pass-to-qsave_program] -c [src-files] ` can be used to create a **saved state**.
    - source code are compiled to some IR
- Each library directory must hold a file `INDEX.pl`that contains an index to all library files in the directory.

File and Path: 

- file extension name shall be `.pl` or `.pro`
- write paths as `path/to/my-code` and use `prolog_to_os_filename/2` to convert to system paths


### Source File Type:

| type        | notes                                    | load                           |
| ----------- | ---------------------------------------- | ------------------------------ |
| _tradition_ | no modules                               | `consult/1`, `ensure_loaded/1` |
| _module_    | starts with module decl                  | `use_module/[1,2]`             |
| _include_   | used as headers, included as source text | `include/1`                    | 


### Path

1. `absolution_file_name(+Rel, -Abs)` relative path to absolute path
2. `absolution_file_name(+Spec, -Abs, +Options)`
    - `+Spec` can be _abs_/_relative_ path or a term with type `Alias(Relative)`, e.g., `library(list)`


## Some Common Predicates

### Basic Logic 

| Predicate | Meaning        | Example |
| --------- | -------------- | ------- |
| `true/0`  |                |         |
| `false/0` |                |         |
| `(',')/2` | and/conjuction |         |
| `(';')/2` | or/disjuction  |         |
| `(=)/2`   | unification    |         |
| `dif/2`   | Disequality               |         |

### Arithmetic

#todo

| Predicate | Meaning | Example |
| --------- | ------- | ------- |
| `(#>)/2`  |         |         |
### Debug Predicates

| Predicate                        | Meaning                               | Example                         |
| -------------------------------- | ------------------------------------- | ------------------------------- |
| `trace/[0,1,2]` `notrace/0`      | set prolog engine to trace/debug mode |                                 |
| `gtrace/0`                       | activate graphic debugger             |                                 |
| `tdebug/0`                       | debug any thread                      |                                 |
| `leash/1`                        | select ports to pause                 | `leash(-all).`, `leash(+fail).` |
| `visible/1`                      | set ports shown by debugger           |                                 |
| `spy/1`, `nospy/1`, `nospyall/0` | set _spy point_ at predicate          | `spy(is_a/2).`                  |
| `set_breakpoint/[4,5]`,          |                                       |                                 |
| `set_brekpoint_condition/2`      |                                       |                                 |

- _spy point_ is a __predicate__ where debugger will not active before the _spy point_ is hit.
- _break point_ is a __position__ will not be triggered if not in _debug mode_. _Breakpoints_ allow for turning on trace mode when a specific source file, line number, and character in that line are hit.



### Interpreter friends

| Predicate                                 | Meaning                           |
| ----------------------------------------- | --------------------------------- |
| `consult/1` or `[file]`                   | load file content                 |
| `pwd/0`, `cd/0`, `ls/0`                   | directory commands                |
| `edit/1`                                  | start default editor              |
| `make/0`                                  | update, compile edited file       |
| `listing/1`, `listing/0`                  | decompile a file (entire program) |
| `statistics/0`                            | statistics of the interpreter     |

- `[user].` fake buffer to add facts in interpreter

### Help:
| Predicate          | Meaning           |
| ------------------ | ----------------- |
| `help/1`           | help on a topic   |
| `apropos/1`        | search predicates |
| `library(explain)` |                   |

### Type Verification

| Predicate                                        | Type             |
| ------------------------------------------------ | ---------------- |
| `var/1`, `nonvar/1`                              | is variable      |
| `integer/1`, `float/1`, `rational/1`, `number/1` |                  |
| `atom/1`                                         |                  |
| `blob/1`                                         |                  |
| `string/1`                                       |                  |
| `atomic/1`, `compound/1`                         |                  |
| `callable/1`                                     | see `call/[1,2]` |
| `ground/1`                                       |                  |
| `cylic_term/1`, `acylic_term/1`                  |                  |
| `is_dict/1`                                      | if is a _struct_, see [[#Basic Types swi-prolog extensions\|types]]                |


| Predicate                   | Meaning                                    |
| --------------------------- | ------------------------------------------ |
| `halt/0`                    | stop prolog                                |
| `functor/3`, `arg/3`, `=..` | extract functor/arg info from complex term |
| `op/3`                      | define operators                           |
  

## Predicate Description

[document](https://www.swi-prolog.org/pldoc/man?section=preddesc), [cheat sheet](https://github.com/dtonhofer/prolog_notes/tree/master/swipl_notes/about_mode_indicators)

predicate indicators:
  
  - `[<module>:]<name>/arity`
  - `[<module>:]<name>//arity`, see [[#DCG Definite Clause Grammas|DCG rule]].

### Argument Mode Indicator

| symbol | meaning                                                            |
| ------ | ------------------------------------------------------------------ |
| `++`   | _input_ arguments must be _grounded_                               |
| `+`    | _input_, conform to some _informal spec_ in doc, can be ungrounded |
| `-`    | _output_ argument, may not be _bounded_                            |
| `--`   | _output_ argument, must be _bounded_                               |
| `?`    | can be both _input_ or _output_                                    |
| `:`    | _meta argument_ e.g., term can be called as a _goal_               |
| `@`    | _static_, usually for type check                                   |
| `!`    | _mutable_ argument                                                 |


## Basic Types (swi-prolog extensions)

1. _List_, `[]` empty list, `[X|XS]` cons
2. _String_ is enclosed in double quotes, `"This is a String"`.
3. _List of Chars_ is enclosed in back quotes.
4. _Atoms_ can be enclosed in single quotes
5. **Structure** or **Dict**, _Tag_ is either an _Atom_ or a _Variable_, _Key_ is either _Atom_ or small integer
    `Tag{Key1:Value1, Key2:Value2, ...}`
    - _Function_ or _Method_ : 
        - `.(+Dict, +Function, -Result)`
        - `X = dict.Method(arg1, arg2)` equivalent to `Method(arg1, arg2, dict, X)`
    - Lookup is order $log(N)$, 
    - Adding values, deleting values and merging with other dicts has order $N$.
    - `library(dicts)`
6. `library(assoc)`  defines a _AVL binary tree_;
7. `library(rbtrees)` implements _red-black tree_.
8. `library(option)`
9. `library(record)` utility predicates for manipulate compound terms.



# Difference List

> The key idea underlying _difference lists_ is to represent the information/data **not as a single list, but as the difference between two lists**.

For instance transit 
```prolog
r3(Z) :- append(X, Y, Z) , r2(X), r1(Y)
```
to
```prolog
r3(X, Z) :- r2(X, Y), r1(Y, Z)
```



# SSU (Single Side Unification)

```prolog
%   +--+---- input term
%   |  |
%   v  v
max(X, Y, X)     % this violates rule 2
%         ^
%         |
%         +--- goal term 

% the prob of this impl
% is that "cut makes the patten matching not exaustive"
max(X,Y,X) :- X >= Y, !.  
max(_,Y,Y).
```

>[!NOTE] _The Craft of Prolog_
> -   Structure every clause as `Head :- Guard, !, Body`. Every clause has the cut as early as possible. Guard can be empty. The last clause often does not need a cut.
> -   Avoid _head unification_ binds values in the _goal term_. 

```prolog
Head, Guard => Body       % a Picat rule
Head :- Guard, !, Body    % equivalent Neck rule
```


# Tabling (Memo)

1. Re-evaluation of a tabled predicate is avoided by _memoizing_ the answers.
2. _Left recursion_, a goal calling a _variant_ of itself recursively and thus _looping_ under the normal Prolog SLD resolution is avoided by suspending the variant call and resuming it with answers from the table.
3. _mode directed tabling_ or _answer subsumption_:
  

# DCG (Definite Clause Grammas)

https://github.com/Anniepoo/swipldcgtut/blob/master/dcgcourse.adoc

> So, what are DCGs? Quite simply, a nice _notation_ for writing grammars that hides the underlying _difference list_ variables.
>
> From: [Learn Prolog Now](http://learnprolognow.com)

## Gramma Rule

```prolog
Head --> Body
```
- A _DCG head_ with functor `f` and arity `N` is referred as `f//N`, to distinguish from `f/N` which refers to a _Predicate_ 



# References & TODOs

[The Power of Prolog](https://www.metalevel.at/prolog)

- resolution (resolution refutation) strategies
  - SLDNF (SLD with negation as finite failure)
  - SLG (tabling)
  - iterative deepening 

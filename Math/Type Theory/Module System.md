# 1 Basic Concepts

- A **program** is a collection of bindings of module names to modules. One of them is specified as the **root** (main entry point).
- One module in a program may refer to another by using the latter’s name in an **external reference**.
- To support separate compilation, the dependency of one module on another is mediated by a **signature** (or **interface**) that describes the _externally visible components_ of the latter module. 
  - (parallel compilation) A signature must be sufficiently expressive as to enable clients of a module to be compiled without reference to its implementation. 
  - This information typically includes type declarations for procedures and variables and the definitions of type variables.


```haskell
data Prog = [Binding]

data Binding 
  = ModBinding Name Mod (Maybe Sig) 
  | SigBinding Name Sig

data Mod 
  = ModVar Name
  | ModBasic [CompBinding]

data Sig 
  = SigVar Name 
  | SigDef [CompDecl]

data CompBinding 
  = TyBinding Name Ty
  | ValBinding Name Term

data CompDecl 
  = TyDecl Name Ty
  | ValDecl Name Ty
```

- The scope of a `Binding` in a program is the remainder of the program following that binding
- A _basic module consists_ of a sequence of component bindings, which are either 
  - A _type binding_ is a binding of a type variable to a type expression; or 
  - A _value binding_ binds a run-time entity to a term variable. These entities may include procedures, classes, objects, mutable reference cells, and other structures from the core language
  - Each `Binding` has both a _label_ (_internal name_), which is underlined, and a _variable_ (_external name_), which is not. 
    - (_internal reference_) The variable governs references to that binding within the module. Internal names are bound variables whose scope is the rest of the module in which they occur.
    - (_external reference_) the label governs reference from outside of the module. The external name of a component of a module cannot be renamed without affecting the entire program in which that module occurs.

>[!def] initialiser
> The notion of an initialiser for a module arises here as a _value binding_ whose right-hand side has a _side-effect_ (initialising the module’s internal state) when evaluated

## 1.1 Module Signature

>[!def] implement a signature
> A module $M$ implements a signature $I$ if 
> 1. for each _type component_ declared in $I$ must be matched by a type binding in $M$ of the same kind and _equivalent_ definition.
> 2. for each _value component_ declared in $I$ must be matched by a value bind in $M$ with a _subtype_ of the type specified in $I$.
> 
> N.B. _type component_ implementation cannot be subtype of the signature due to contravariant positions.

>[!def] structural signature matching
> Given 2 signatures $I$ and $J$, $I <: J$ if and only if for all module $M$, $M: I \implies M: J$.
> 1. Every type declaration in $J$ must be matched by a corresponding type declaration in $I$. Moreover, their definitions must be _equivalent_, taking into account the preceding type declarations in $I$.
> 2. Every value declaration in $J$ must be matched by a corresponding value declaration in $I$. Moreover, the type declared in $I$ must be a _subtype_ of that declared in $J$, taking account of the preceding type bindings in $I$.

>[!def] principle signature
> The _principal signature_ of a module, when it exists, is the _most precise signature_ that the module implements. 
> If a module $M$ has a principal signature $I_M$, then $M$ implements another signature $I$ exactly when $I_M$ matches $I$, i.e. $I_M <: I$.

The inference of principle signature can be quite complicated, the reason is that module provides some functionality of access control (even with types), while the type of a public value must be public.

>[!def] avoidance problem
In general, the principal signature of a _let expression_ of the form `let module m = M in M'` is the least signature for `M'` that does not involve the bound module variable `m`. 
In other words, type signature of components of `M'` shall not mention `m`, i.e. _avoid_ `m`. 
>
The reason of this choice is that we do not want types of the local module `M`, which are local, get exposed through exported values of `M'`


>[!def] transparent, opaque & translucent
> 1. a _concrete/transparent type signature_ reveal the _definition_ (not only existence) of a type component. 
> 2. an _abstract/opaque type signature_ revealing the _existence_, but not the definition of a type component.
> 3. signature that can be either _concrete_ or _abstract_ are said to be _translucent_

# 2 Compilation

## 2.1 Cutoff Compilation

if the source code of a module has changed, but its signature has not, then there is no need to recompile modules that depend on it—recompilation may be “cut off” at that point.
## 2.2 Separate Compilation

In a separate compilation system, _the programmer states signature assumptions for each of the external references in a module._ 
This is typically achieved by “import” declarations that state such assumptions. The module is compiled relative to these assumptions, independently of whether the implementation of the externally referenced modules is available
## 2.3 Incremental Compilation

In an incremental compilation system, it is not necessary to specify the signatures of externally referenced modules. 
Instead, _the compiler consults the implementation of a module to determine its signature_, which is used for compiling any module that depends on it.


# 3 Static Linking

## 3.1 definite references only

>[!def] definite reference
> all the external references to a given module $m$, _everywhere in a given set of modules_, are guaranteed to be resolved to the same module value at link time—that is, external references are definite.

A linker assembles a complete program from a collection of module bindings, called the _linking context_.

>[!def] dependency relation of modules
> _dependency relation_ consisting of _reference dependencies_ together with its _initialisation dependencies_.
>
> 1. If a module $N$ contains an external reference $m$ to a module $M$, then $N$ is said to contain a _reference dependency_ on $M$. _Signatures_ may also contain reference dependencies on modules
> 2. _Initialisation dependencies_ arise when the evaluation of one module (_initialiser_) is materially affected by the evaluation of another. Initialisation dependencies cannot always be determined by inspection; and thus needs to be _explicitly specified_ by the programmer.

## 3.2 indefinite reference (parametric module)

# 4 First class module

## 4.1 Determinacy

>[!def] determinate & indeterminate 
> a _module expression_ is **determinate** if its _type components_ are _statically known_ and can be selected without fear of violating safety or _representation independence_.

consider the following expression 
```ocaml
module type I = 
  sig 
    type X  
    val f : X -> int
    val x : X
  end
module M1 : I = struct type X = int ... end
module M2 : I = struct type X = bool ... end

let flipState : bool ref = ref false 
let flip () : bool = begin
  flipState := not (!flipState)
  !flipState
end

let main = 
  (if flip () then M1 else M2).f (
    (if flip () then M1 else M2).x
  )
```

evaluating `main` may trigger a runtime error, thus, the soundness of type system requires main to be ill-typed.

the reason is the module express `if flip() then M1 else M2` is not determinate.

>[!remark]
>1. module value and module variable are the **only** _determinate_ module expression;
>3. more specifically, sealed modules are _indeterminate_.

>[!remark]
>_indeterminate_ module expression do not allow type selection (`M.T`, where `T` is a type)

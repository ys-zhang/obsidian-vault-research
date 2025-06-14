
# 1 Type 

## 1.1 ADT

```ocaml
type my_int = int (* type alias *)

type 'a maybe = Just of 'a | Nothing

type 'a tree 
  = Leaf 
  | Node of 'a tree * 'a * 'a tree
```

`match with` is the Haskell `case of` equivalent in OCaml 

```ocaml
let rec size (tree : 'a tree) = 
  match tree with 
  | Leaf -> 0
  | Node (left, _, right) -> size left + 1 + size right
```
## 1.2 Records

>[!tldr] record is named tuple
> A _record type_ can be thought of as _a tuple where the individual fields are named_, rather than being defined positionally

```ocaml
(* this is actually a type alias *)
type animal = 
  { name: string;
    color: string;
    legs: int;
  }

let cow: animal = 
   {  name = "cow";
      color = "black and white";
      legs = 4; 
   }

(* mutable marks a field is mutable *)
type 'a ref = { mutable contents : 'a }
```

the following record syntax supports the equivalent Haskell Language extension:
1. `NamedFieldPuns` 
2. `OverloadedRecordDot`

```ocaml
(* field-punning is exactly `NamedFieldPuns` in Haskell *)
let magnitude { x; y } = Float.sqrt (x **. 2. +. y **. 2.)

let cowName = cow.name
```

# 2 Module and Functor

## 2.1 Module as a module system 

- At its simplest, you can think of a module as a collection of definitions that are stored within a _namespace_.
- Each source file is compiled down into a module whose name is derived from the name of the file.
- Module operations
  - `include` **re-exports** the components of the module in the current structure: the module you are in will contain all definitions that are in `Ppx_core`.
  - `open` makes the components of the module **directly accessible**. Instead of typing `Core.Std.element` you can just type `element`.
  - `#require` is a `Topfind` command that finds a library and loads it, making its modules accessible to you.
  - `#use` behave as if **copying a full file** directly into your toplevel.


```ocaml 
(* the open keyword brings the namespace of M into the current module *)
open M 

(* a module can be opened locally *)
let f x = let open N in 
          ...
```


>[!std lib convention]
> - _A module for (almost) every data type_
> - _Put `t` first_, abstract types shall be placed first
> - Functions that routinely throw an exception should end in `_exn`

## 2.2 module language

>[!tldr]
> - module language makes the dependency _parametrisable_, i.e. `B` can import _an undetermined module with a determined signature_. 

Any module system supports _importing modules_ which makes a _dependency relation_ among modules.

> `B`  imports `A` makes `B` depends on `A`, dubbed as `B => A`

```ocaml
module type X_int = sig 
  val x : int  
  
end

module B (A: X_int) : X_int = struct 
  let x = A.x + 1
end
```

>[!note] signature
> signature is a type system for modules, which is _duck type_ just like [[#1.2 Records|records]]. Notice, you can achieve "inheritance" buy using **signature include**

```ocaml
module type Xt = sig 
  (** t is an abstruct type, not a type parameter *)
  type t  
  val t : t
  val someInt : int
end

(**
   X's signature is not exactly the same as Xt
   but it covers Xt, thus can be used when ever 
   and Xt is required see (*1*)
*)
module X = struct 
  type t = int * int 
  let t = (1, 1)
  let someInt = 0
  let x = 10
end

module Y (M: Xt) = struct 
 let y = M.someInt + 1. 
end

let () = 
  let module X' = Y(X) in                (*1*)
  Printf.printf "%d" X'.y
```

## 2.3 First Class Modules

>[!def]
> First-class modules are ordinary values that can be created from and converted back to regular modules. 
>
> You can use variables to hold first class modules and functions to manipulate them.

1. using variable to hold a free module
```ocaml
module type X_int = sig val x: int end

(* This is a regular module *)
module Three: X_int = struct let x = 3 end

(* This defines a first class module three, 
   by converting a regular one to a first class
   one
 *)
let three = (module Three: X_int)

module New_Three = (val three: X_int)
```
2. using function to modify free modules
```ocaml
let to_int m =
  let module M = (val m : X_int) in
  M.x;;

let plus m1 m2 =
  (module struct
    let x = to_int m1 + to_int m2
  end : X_int);;
```

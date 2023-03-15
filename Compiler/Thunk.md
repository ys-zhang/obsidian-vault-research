>  A function that is used just to delay computation, and in particular one that takes `unit` as input, is called a _thunk_.

# Implementation of Sequence

```ocaml
module SeqUseThunk = struct 
  type 'a thunk = unit -> 'a
  type 'a seq = Cons of 'a * 'a seq thunk 
  
  let nat: int seq = 
    let rec from n = Cons (n, fun () -> from (n+1)) 
    in from 0
  let repeat (x: 'a): 'a seq = Cons (n, fun () -> repeat n)
  let head (Cons (x, _)) = x
  let tail (Cons (_, xs)) = xs ()
end
```


# Lazy

```ocaml
let rec fibs = 
  Cons (1, fun () -> 
    Cons(1, fun () ->
      add fibs (tail fibs)))
```

Every time we force the computation of the next element, it required recomputing all the previous elements, twice: once for `fibs` and once for `tl fibs` in the last line of the definition.

The idea to solve this is to make `Thunk` lazy
```ocaml
module Lazy :
  sig
    type 'a t = 'a lazy_t
    val force : 'a t -> 'a
    ...
  end
```

> A value of type `'a Lazy.t` is a value of type `'a` whose computation has been delayed.
> 
> Intuitively, the language is being _lazy_ about evaluating it: it won’t be computed until specifically _demanded_. 
> The way that _demand_ is expressed with by _forcing_ the evaluation with `Lazy.force`, which takes the `'a Lazy.t` and causes the `'a` inside it to finally be produced.
> 
> The first time a lazy value is forced, the computation might take a long time. But the result is _cached_ aka _memoized_, and any subsequent time that lazy value is forced, the memoized result will be returned immediately without recomputing it.

The `Lazy` module doesn’t contain a function that produces a `'a Lazy.t`. Instead, there is a keyword built-in to the OCaml syntax that does it: `lazy e`.
-   **Syntax:**  `lazy e`
-   **Static semantics:** If `e : u`, then `lazy e : u Lazy.t`.
-   **Dynamic semantics:** `lazy e` does not evaluate `e` to a value. Instead it produces a _suspension_ that, when later forced, will evaluate `e` to a value `v` and return `v`. Moreover, that suspension remembers that `v` is its forced value. And if the suspension is ever forced again, it immediately returns `v` instead of recomputing it.


>[!note]
>OCaml’s usual evaluation strategy is _eager_ aka _strict_: 
>
>it always evaluate an argument before function application. If you want a value to be computed lazily, you must specifically request that with the `lazy` keyword. 
>
>Other function languages, notably Haskell, are lazy by default. Laziness can be pleasant when programming with infinite data structures. But lazy evaluation makes it harder to reason about space and time, and it has unpleasant interactions with side effects.


```ocaml
let rec lazy_fibs = 
```


```ocaml
module SeqUseLazy = struct 
  type 'a seq = Cons of 'a * 'a seq Lazy.t 
  
  let nat: int seq = 
    let rec from n = Cons (n, lazy from (n+1)) 
    in from 0
  let repeat (x: 'a): 'a seq = Cons (n, fun () -> repeat n)
  let head (Cons (x, _)) = x
  let tail (Cons (_, xs)) = Lazy.force xs
end
```


# Haskell 

>[!note] 
>There is a slight difference between _laziness_ and _nonstrictness_. **Nonstrict semantics** refers to a given property of Haskell programs that you can rely on: nothing will be evaluated until it is needed. **Lazy evaluation** is how you implement nonstrictness using a device called **thunks**

> **Thunks** are _unevaluated values_ with a _recipe_ that explains how to evaluate them.

Evaluation can be levelled:
![](https://upload.wikimedia.org/wikipedia/commons/f/fc/Thunk-layers.png)

Performing any degree of evaluation on a value is sometimes called **forcing**that value.

if we have a constructor with strict components (annotated with an exclamation mark, as with `data MaybeS a = NothingS | JustS !a`), these components become evaluated as soon as we evaluate the level above. I.e. we can never have `JustS *thunk*` — as soon as we get to this level, the strictness annotation on the component of `JustS` forces us to evaluate the component part.

> Nothing gets evaluated until it is needed (in fact, the _only_ place that Haskell values get evaluated is in pattern matches and inside certain primitive IO functions) .

some functions evaluate their arguments more fully than others. Given two functions of one parameter, `f` and `g`, we say `f` is stricter than `g` if `f x` evaluates `x` to a deeper level than `g x`.

Often, we only care about WHNF, so a function that evaluates its argument to at least WHNF is called _strict_ and one that performs no evaluation is _lazy_.

Generally, in the implementation the thunk is really just a pointer to a piece of (usually static) code, plus another pointer to the data the code should work on. If the entity computed by the thunk is larger than the pointer to the code and the associated data, then a thunk wins out in memory usage.




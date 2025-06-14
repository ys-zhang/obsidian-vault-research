#dynamic-checking

# 1 Contracts for higher-order functions

>[!def] carrier
> a

## 1.1 Embellish types with contracts 

## 1.2 Contract Monitoring in Runtime

```haskell
data Obligation = Obligation
  { base :: Expr
  -- ^ the base expression
  , contract :: Expr
  -- ^ the contract expression that `base` 
  -- obliged to hold
  , blameForInput :: Party
  , blameForOutput :: Party
  , srcLoc :: SrcLoc
  }
```

# 2 Oh Lord, Pls don't let contracts be misunderstood 


<iframe width="100%" height="315" src="https://www.youtube.com/embed/yG9kc9CNSjs?si=QKOBqq6KSS5L7EsB" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

the paper gives the design space of contract system, a _contract system_ consists of 3 parts:
1. _interposition layer_
2. _contract attachment mechanism_
3. a suit of _contract combinators_

# 3 Contract in Racket
this is a brief summary of design of _contracts_ in _racket_.

contracts are basically runtime checks enforced on values and collection of values, including functions, structs and objects.

1. contract checking on every operation is expensive;
2. how to compose contracts;
3. how to attach contracts to values:

## 3.1 Contract Domain and Boundary  

>[!tldr]
>rackets contract system uses _module_ as default _domain_. 
>
>Contract do **not** perform _runtime check_ when the value it governed is used **inside** its _domain_.

To solve the efficiency problem, every contract is associated with a domain, which can be either:
1. the _module_ where the value governed by the contract is defined
2. the _definition_ of the value governed by the contract.

## 3.2 Blame

>[!def] client-server 
> a contract is simply a _first-order logic clause_ that states if some _premise_ holds, then some property holds. Each contract is associated with 2 parties:
> - the _server/supplier_, guarantees the result property if premises holds.
> - the _client_ is responsible for the _premise_ to hold. 

> Thus, if contract is not fulfilled, which party to blame?

>[!def] positive and negative position
> _positive_ positions are where the _server_ is responsible to guarantee,
> _negative_  positions are where the _client_ is responsible to guarantee.
>
> positive and negative positions are also called _covariant_ and _contra-variant_ in category theory;

Given a contract `a -> Bool`, it introduces a subset of `a`, which can be seen as a _subtype_ of `a`. the form 

```racket
(flat-named-contract 
   (name : 'symbol) 
   (predicate : 'a -> Bool))
```

## 3.3 Composition

### 3.3.1 and/or

since contracts are basically predicates maps to truth value. we have logic connectives as contract combinators. such as `or/c`, `and/c`, `any/c`, `any`

### 3.3.2 implies

racket provides three contract combinators for _functions_
1. (`->`) `pre-condition -> post-condition`, notice that _post condition_ is irrelevant to function input
2. (`->*`) deals with function with _optional argument_
3. (`->i`) The “i” stands for an _indy dependent contract_, meaning the contract for the function range depends on the value of the argument. This is quite like _dependent types_


### 3.3.3 Checking state changes

the interesting part is `->i` supports checking state changes.

>[!question] 
>given an impure function, how to ensure the call back do not break invariants of some state?

To solve the problem, we need to cache the state before the function call and after the function, compare the old state with the new one.

the contract constructor `->i` accepts a _range contract constructor_, when marked with `_`, will be evaluated before the function call.

```racket
(define/contract func-modifies-state 
  (->i ([arg constract-for-arg])
       [_ (arg) 
          (let ([old-state ...])
               ; all computation before the following
               ; closure is executed before 
               ; `func-modifies-state` get called
               (lambda (result) 
                  ; the closuer depends on 
                  ; the result of `func-modifies-state`
                  ; and thus, is executed after the 
                  ; function call
                  (let ([new-state ...])
                       (state-invariant-holds? 
                          old-state
                          new-state))))])
  (lambda (arg) ...))
```

# 4 References

1. Dimoulas, C., New, M. S., Findler, R. B., & Felleisen, M. (2016). Oh Lord, please don’t let contracts be misunderstood (functional pearl). _Proceedings of the 21st ACM SIGPLAN International Conference on Functional Programming_, 117–131. [https://doi.org/10.1145/2951913.2951930](https://doi.org/10.1145/2951913.2951930)
2. [Racket language guide for contract programming](https://docs.racket-lang.org/guide/contracts.html)
3. Findler, R. B., & Felleisen, M. (2002). Contracts for higher-order functions. _Proceedings of the Seventh ACM SIGPLAN International Conference on Functional Programming_, 48–59. [https://doi.org/10.1145/581478.581484](https://doi.org/10.1145/581478.581484)

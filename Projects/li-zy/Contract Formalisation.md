# Modelling Requirements

> [!note]
> A _contract_ is a legally binding agreement between two or more parties. The agreement is a normative description of commitments between the _parties_ of the contract, that is a contract describes the expected _actions_ to be performed by the participants of the contract.
> 
> Each _action / commitment_ is associated with a _timestamp_ (action starts), a _time span_ (time cost of the action), a _premise_ and a _normative requirements / deontic status_.


1. Contract model, contract language, and a formal semantics.
2. Contract participants.
3. <mark class="hltr-pink">(Conditional) commitments.</mark>
4. <mark class="hltr-pink">History-sensitive commitments.</mark> (3 times illness)
5. <mark class="hltr-pink">Absolute and relative temporal constraints</mark>.
6. Reparation clauses.
7. Instantaneous and continuous actions. (actions consume/not consume time)
8. Potentially infinite and repetitive contracts. (Renewable Term Insurance)    `can be modelled by a quitting term`
9. Time-varying, external dependencies (observables).
10. In-place expressions.


# Contract Combination

we can use ocaml-like module and functors to model hierarchical  contract combination
```ocaml

(* A term states that at each end of month the policy holder can have 
  a dividant (may prop to its accounttvalue at the begining of the month ) *)
module type DividantTermType (InsurantModel: InsurantModelType, DividantModel: DividantModelType) = sig
  (* insurant_observations are like Modelling of DeathEvents, CriticalIllness Events, LapseEvnets .... *)
  module Insurant = InsurantModel(insurant_observations...)
  (* this outputs an observable of account value, this is actually a valuation model *)
  module AccountValueModel: sig 
  end

  type t (* record holding dividant actions *)
end


module type AccountValueTermType (
  DividantTerm: sig AccountValueModel: AccountValueModelType end
  ...
) = sig 
  (* this shall be created from ContractTermA *)
  module AccountValueModel: AccountValueModelType = DividantTerm(...).AccountValueModel,
  let accountValue = AccountValueModel.get ()
  (* output and event  *)
end

```



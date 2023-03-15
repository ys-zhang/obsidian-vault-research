# Context

> Solid provides a Context API to pass data around without relying on passing through props.
> 
> This is useful for sharing `Signals` and `Stores`.

Two basic operations for contexts:
1. register a context
2. retrieve a registed context

| Op       | Solid                  | Leptos                     |
| -------- | ---------------------- | -------------------------- |
| register | a _Provider_ Component | `provide_context<CTX>(scp: Scope, ctx: CTX)` function |
| retrieve | `useContext(ContextType)` function  | `use_context<CTX>(scp: Scope)` function                           |

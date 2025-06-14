#Haskell 

# 1 Basic Concepts

1. **Module** is what you think it is.
2. **Signature** is a “type signature of module”.
3. **Unit** is a collection of _modules_ and _signatures_. Think _library_.
4. _Units_ can be **open**, contain “holes” (specified by _signatures_) to be filled by other _modules_. See `OpenUnitId` in Cabal docs.
5. **Mixin** is a _unit_ with “inputs” and “outputs” possibly _renamed_. (Grenrus, 2019)


# 2 FAQ

1. No need to import(or depends on) the signature(the containing unit) when writing a module implementing it.[^faq-1]
2. Implementation and signature share the same name.[^faq-2]
3. Usually different implementation goes into different packages.[^faq-3]
    - Use [reexported-modules](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-library-reexported-modules) in cabal package description.
    - Use [mixins](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-mixins) for multiple implementation in the same package, which allows _rename modules_ from the package.

[^faq-1]:“we need to export all types and classes with the same names and kinds, and all functions with the same names and types from the module with the implementation.” (Kovanikov, 2018)
[^faq-2]:“Having reexported-modules field is a vital part of our interface. _Backpack matches the name of the signature module with the name of the module with the implementation_.” (Kovanikov, 2018)
[^faq-3]:“we don’t want to have extra dependencies if the user is only interested in a single implementation. Also, it wouldn’t work well with [reexported-modules](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-library-reexported-modules) approach since we can’t reexport two different modules under same name.” (Kovanikov, 2018)

>[!note] the `mixins` cabal stanza
> - mixins must be mentioned in `build-depends`
> - rename will have a side-effect of hiding other modules
>
> the _require_ clause instantiate a signature with a implementation module. 

## 2.1 Instantiate parametrised packages

Unlike OCaml, Backpack operates slightly differently with a concept of mix-in linking, where you provide an implementation of `SomeSig` simply by bringing another module into scope with _the same name_ as the **requirement**.

>[!example]
>if you had a package `str-impl` that provided a module named `Str`, instantiating `parametrized-by-str` is as simple as just depending on both `str-impl` and `parametrized-by-str`

> [!example]
> If you have a requirement `Str` and an implementation `Data.Text`, you can line up the names in one of two ways:
>
> 1. Rename the requirement to match the implementation:
>    ```cabal
>    mixins: parametrized-by-str requires (Str as Data.Text)
>    ```
> 1. Rename the implementation to match the requirement:
>    ```cabal
>    mixins: text (Data.Text as Str)
>    ```
# 3 References

1. Carrete, D. D. (2024). _Danidiaz/really-small-backpack-example_ [Haskell]. [https://github.com/danidiaz/really-small-backpack-example](https://github.com/danidiaz/really-small-backpack-example) (Original work published 2018)
2. Grenrus, O. (2019). _Unrolling data with Backpack_. [https://well-typed.com/blog/2019/11/unrolling-data-with-backpack/](https://well-typed.com/blog/2019/11/unrolling-data-with-backpack/)
3. Kilpatrick, S., Dreyer, D., Peyton Jones, S., & Marlow, S. (2014). Backpack: Retrofitting Haskell with interfaces. _SIGPLAN Not._, _49_(1), 19–31. [https://doi.org/10.1145/2578855.2535884](https://doi.org/10.1145/2578855.2535884)
4. Kovanikov, K.-D. (2018). _Picnic: Put containers into a backpack_. [https://kowainik.github.io/posts/2018-08-19-picnic-put-containers-into-a-backpack](https://kowainik.github.io/posts/2018-08-19-picnic-put-containers-into-a-backpack)
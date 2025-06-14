


# Events

```js
document
  .querySelectorAll('textField[data-nid=...]')
  .forEach(elem => elem.dispathEvent(new Event('event-name')));
```

## Refs

1. [creating and trigger events](https://developer.mozilla.org/en-US/docs/Web/Events/Creating_and_triggering_events)


# Morph

_morph_ is about merging a new version of DOM to the existing one.

## Alpine morph

```js
htmx.defineExtension('alpine-morph', {
    isInlineSwap: function (swapStyle) {
        return swapStyle === 'morph';
    },
    handleSwap: function (swapStyle, target, fragment) {
        if (swapStyle === 'morph') {
            if (fragment.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
                Alpine.morph(target, fragment.firstElementChild);
                return [target];
            } else {
                Alpine.morph(target, fragment.outerHTML);
                return [target];
            }
        }
    }
});
```

# Hyperscript

> hyperscript is **event oriented**, any hyperscript you return to htmx will be automatically initialized without any additional work on your part.


> [!def] hyperscript ast 
>  - A hyperscript script consists of a series of _"features"_, the most common of which is an event handler. 
>  - The body of a _feature_ then consists of a series of _"commands"_, which are often called statements in other languages. 
>  - The _commands_ may include one or more _"expressions"_
>
> ```haskell
> type Script = [Feature]
> data Feature 
>   = On [Command]  -- ^ event handler feature
>
> data Command 
>   = Set Scope Name Expr 
>   -- ^ `set` or `put`
>   | SetAttr (Expr Ref) Expr
>   -- ^ `set @my-attr to 10`
> 
> data Scope 
>   = Global 
>   -- ^ `$globVar` or `global globVar`
>   | Element  
>   -- ^ share across all features on the elem
>   -- `:elemVar` or `element elemVar`
>   | Local    
>   -- ^ local to the current feature
>   -- `local locVar`
>
> data Expr a where
>   -- | `1.1`
>   Num :: (forall a. Num a => a) -> Expr Number
>   -- | `"hello"`
>   Str :: String -> Expr String
>   -- | `[1, 2, 3]`
>   Arr :: [Expr a] -> Expr [a]
>   -- | `{name: "Joe", age: 35}`
>   Obj :: JSON -> Expr (Object a)
>   -- | html id ref `#foo`
>   Id :: String -> Expr Ref
>   -- | html class ref `.tabs`
>   Class :: String -> Expr [Ref]
>   -- | html query ref `<div/>` 
>   Query :: CssSelector -> Expr [Ref]
>   -- | html attribute ref `@count` 
>   Attr :: String -> Expr [Ref]
> ```



## Keywords

| cat                | keyword          | semantics                                                                                   |
| ------------------ | ---------------- | ------------------------------------------------------------------------------------------- |
| separator          | `then`           | separate commands when on a line                                                            |
| separator          | `end`            | close a command block[^kw-end]<br>                                                          |
| white              | `the/a/an`       | equiv to a white space                                                                      |
| scope              | `global`         | `global globVar` equiv to `$globVar`                                                        |
| scope              | `local`          | `local locVar`                                                                              |
| scope              | `element`        | `element elemVar` equiv to `:elemVar`                                                       |
| prop-expr[^p-expr] | `'s`             | object field getter/setter, can be omitted when used with `my/its`                          |
| prop-expr          | `of`             | `log the name of her-dog`                                                                   |
| sp name            | `it/its/result`  | refers to the result of _prev command_                                                      |
| sp name            | `me/my/I`        | the _element_ runs the _event handler_                                                      |
| sp name            | `event`          | the [`Event`](https://developer.mozilla.org/en-US/docs/Web/API/Event) triggers the handler  |
| sp name            | `target`         | [`Event.target`](https://developer.mozilla.org/en-US/docs/Web/API/Event/target)             |
| sp name            | `detail`         | [`CustomEvent.detail`](https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent/detail) |
| sp name            | `sender`         | the _element_ that sends the event                                                          |
| sp name            | `body`           | the `<body/>` element                                                                       |
| sp name            | arr index        | sp arr prop: `first`, `last`, `random`                                                      |
| sp name            | `initial`        | the initial value of an elements style when the first _transition_ begins.                  |
| bool expr          |                  | `is/is not/am`, `no <cond>`, `matches`, `exists`, `is greater/less than`, `is empty`        |
| ctrl flow          | `if` cmd         | `else/otherwise`, `... unless <cond>`                                                       |
| ctrl flow          | `repeat` cmd     | `repeat for`, `repeat while`, `repeat until`, `repeat forever`, `repeat <num> times`        |
| ctrl flow          | `break/continue` |                                                                                             |

[^kw-end]: can be omitted if the next token starts a new _feature_;
[^p-expr]: possessive expression example `set her-dog to {name: "maymay",age: 6} then log her-dog's name then log the age of her-dog`

## Commands

- `set/put` variable assign
- `make` creates new object
- `add`
- `if`, `repeat`
- `call/get` evaluates an expression, can be used to call a function
- `increment/decrement`

### event handling

- `on` event handler 
- `halt` 
  1. `halt the event`: call `preventDefault()` and `stopPropagation()`
  2. `halt the event's (bubbling | default)`
  3. `halt`, equivalent to `halt the event then exit`
- `send`/`trigger` 

```haskell
data On 
  = On AsyncFlag               -- ^ `every` or not? 
       Event                   -- ^ the event
       QueueStrategy           -- ^ how to queue events
       [Binding]               -- ^ bind event props to local var   
       (Maybe (Event -> Bool)) -- ^ event filter
       [Command]

data Event 
  = HtmlEvent
  | MutationEvent 
  | IntersectionEvent

-- | `on [every] <event>`
data AsyncFlag 
  = Every              -- ^ run handler async 
  | Sync               -- ^ default

-- | `on <event> queue <strategy>`
data QueueStrategy
  = None               -- ^ 0-size channel:  drop all
  | All                -- ^ inifine-size channel: queue all 
  | First              -- ^ 1-size channel: queue first
  | Last               -- ^ 1-size channel: queue last
```


>[!tldr] async transparency
> hyperscript will automatically detect a promise and await on it, if you do not want to await it, use the `async` keyword.

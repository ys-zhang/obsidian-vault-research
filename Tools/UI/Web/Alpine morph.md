#alpinejs 


important functions:
- [`patch(from, to)`](https://github.com/alpinejs/alpine/blob/main/packages/morph/src/morph.js#L31)

```js
function swapElements(from, to) {
    if (shouldSkip(removing, from)) return
    
    let toCloned = to.cloneNode(true)
    
    if (shouldSkip(adding, toCloned)) return
    
    from.replaceWith(toCloned)
    
    removed(from)
    added(toCloned)
}
```

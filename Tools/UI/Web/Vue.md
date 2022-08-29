#web 
#ui 


# Vue

## Some Snippets

Under the hood, Vue compiles the _templates_ into highly-optimized _JavaScript_ code. Combined with the reactivity system.

If you are familiar with _Virtual DOM_ concepts and prefer the raw power of JavaScript, you can also [directly write render functions](https://vuejs.org/guide/extras/render-function.html) instead of templates, with optional JSX support. However, do note that they do not enjoy the same level of compile-time optimizations as templates.

The _double mustachios_ (`{{}}`) interpret the data as _plain text_, not HTML.


## Directives

![[vue-directive-gramma.png]]


## Reactive Model

[Reactivity in Depth | Vue.js (vuejs.org)](https://vuejs.org/guide/extras/reactivity-in-depth.html)

Vue provides two primitive functions to create reactive objects:

1. `reactive(obj)`: for reactive objects
2. `ref(val)`: for reactive primitive value and objects
3. `computed(func)` : create a _computed ref_

`reactive(obj)` is implemented using [[js proxy object]], thus,

1. (_primitive fails_) proxy's target has to be an object, primitive types are not supported, i.e., not reactive;
2. (_deep reactivity_) all getter's of returned proxy that should return an object actually returns the proxy of that object;
3. (_singleton_) there can be only one proxy object of a target object, i.e., 
    - `reactive(target)` returns the same proxy object whenever called;
    - proxy of proxy is the proxy itself.

`ref(val)`  create a "reference" to any value and pass it around without losing reactivity.
1. the _referee_ has to be accessed trough deref `reference.value`;
2. references are automatically dereferenced in templates.
3. if `val` is an object then `reference.value` is the proxy of `val` created through `reactive(val)`

__computed ref__ are similar to _Vue methods_; however, methods are called whenever render happens, but __computed ref__ are only called when its dependencies changed.


### Pseudo-code

```js

function reactive(obj) {
  return new Proxy(obj, {
    get(target, key) {
      track(target, key)    // subscribe the target
      return target[key]
    },
    set(target, key, value) {
      target[key] = value
      trigger(target, key)  // trigger reactive update
    }
  })
}


function ref(value) {
  const refObject = {
    get value() {
      track(refObject, 'value')  // subscribe the target
      return value
    },
    set value(newValue) {
      value = newValue
      trigger(refObject, 'value')  // trigger reactive update
    }
  }
  return refObject
}
```




# Vite




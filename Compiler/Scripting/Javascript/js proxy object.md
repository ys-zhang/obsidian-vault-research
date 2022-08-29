#javascript
#metaprogramming 


[Proxy - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy)
[Proxy Tutorial (archive.org)](https://web.archive.org/web/20171007221059/https://soft.vub.ac.be/~tvcutsem/proxies/)


<iframe width="560" height="315" src="https://www.youtube.com/embed/sClk6aB_CPk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

# Proxy Model

```
+-------+            +---------+             +--------+
| Proxy | ---------> | handler | ----------> | target |
+-------+            +---------+             +--------+
```

the `proxy` object exposes same interface as the `target` object.
all messages passed to the `proxy` object is relayed to the `handler`, which intercepts and customises the reaction or directly delegates to the `target` object.

## Create Proxy

```js
const proxy = new Proxy(target, handler);
```


## Handler Interception

```js
const proxy_target = {};
const proxy = new Proxy(proxy_target, handler);
```

| Operation                       | Traps (intercepted as)                                        |
| ------------------------------- | ------------------------------------------------------------- |
| `proxy[name]`                   | `handler.get(target=proxy_target, prop=name, receiver=proxy)` | 
| `proxy[name] = val`             | `handler.set(target=proxy_target, prop=name, value=val, receiver=proxy)`                               |
| `name in proxy`                 | `handler.has(target=proxy_target, prop=name)`                                           |
| `delete proxy[name]`            | `handler.deleteProperty(target=proxy_target, prop=name)`                                        |
| `Object.keys(proxy)`            | `handler.ownKeys(target=proxy_target)`                                              |







# Example

```js
const target = {
  message1: "hello",
  message2: "everyone"
};

const handler = {
  get(target, prop, receiver) {
    return "world";
  }
};

const proxy2 = new Proxy(target, handler);
```

![[Vue#Pseudo-code]]

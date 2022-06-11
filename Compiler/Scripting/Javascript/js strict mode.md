[Strict mode - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode)

[Transitioning to strict mode - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode/Transitioning_to_strict_mode)

Strict mode makes several changes to normal JavaScript semantics:

1.  Eliminates some JavaScript silent errors by changing them to throw errors.
2.  Fixes mistakes that make it difficult for JavaScript engines to perform optimizations: strict mode code can sometimes be made to run faster than identical code that's not strict mode.
3.  Prohibits some syntax likely to be defined in future versions of ECMAScript.


## Invoking strict mode

strict mode can only apply to *the whole script* or a *function*.

- whole script
```javascript
// Whole-script strict mode syntax
'use strict';
var v = "Hi! I'm a strict mode script!";
```
- only to a function
```javascript

function strict() {
  // Function-level strict mode syntax
  'use strict';
  function nested() { return 'And so am I!'; }
  return "Hi!  I'm a strict mode function!  " + nested();
}

function notStrict() { return "I'm not strict."; }

```


# Strict mode changes some previously-accepted mistakes into errors

```js
'use strict';
                       // Assuming no global variable mistypeVariable exists
mistypeVariable = 17;  // this line throws a ReferenceError due to the
                       // misspelling of variable

// Assignment to a non-writable global
var undefined = 5; // throws a TypeError
var Infinity = 5; // throws a TypeError

// Assignment to a non-writable property
var obj1 = {};
Object.defineProperty(obj1, 'x', { value: 42, writable: false });
obj1.x = 9; // throws a TypeError

// Assignment to a getter-only property
var obj2 = { get x() { return 17; } };
obj2.x = 5; // throws a TypeError

// Assignment to a new property on a non-extensible object
var fixed = {};
Object.preventExtensions(fixed);
fixed.newProp = 'ohai'; // throws a TypeError


delete Object.prototype; // throws a TypeError


```



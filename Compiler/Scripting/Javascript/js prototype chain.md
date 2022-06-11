[Inheritance and the prototype chain - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Inheritance_and_the_prototype_chain)

JavaScript 是动态的，本身不提供一个 class 的实现。即便是在 ES2015/ES6 中引入了 class 关键字，但那也只是语法糖，JavaScript 仍然是基于原型的。

当谈到继承时，JavaScript 只有一种结构：对象。每个实例对象（object）都有一个私有属性（称之为 `__proto__` ）指向它的构造函数的原型对象（prototype）。该原型对象也有一个自己的原型对象（`__proto__`），层层向上直到一个对象的原型对象为 `null`。根据定义，`null` 没有原型，并作为这个原型链中的最后一个环节.


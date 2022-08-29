# Create object

## Creating objects with literal notation
```js
let obj = { property_1:   value_1,    // property_# may be an identifier...
            2:            value_2,    // or a number...
           'property n':  value_n };  // or a string

```


## Creating objects using constructor function

```js
let protocar = {type: "car"}

function Car(make, model, year) {
  this.make = make;
  this.model = model;
  this.year = year;
}

Car.prototype = protocar;

let mycar = new Car('Eagle', 'Talon TSi', 1993); 

Object.getPrototypeOf(mycar); // returns protocar
```


In JavaScript, all functions have a property named `prototype`. When you call a function as a constructor (use the `new` keyword), this property is set as the prototype of the newly constructed object (by convention, in the property named `__proto__`).


![[js-constructor-prototype.png]]

see [[#this object]]


## Creating by `Object.create` method

`Object.create` allows you to *choose the prototype object* for the object you want to create, without having to define a constructor function.

```js
// Animal properties and method encapsulation
var Animal = {
  type: 'Invertebrates',     // Default value of properties
  displayType: function() {  // Method which will display type of Animal
    console.log(this.type);
  }
};

// Create new animal type called animal1
let animal1 = Object.create(Animal);
animal1.displayType();        // Output:Invertebrates

// Create new animal type called Fishes
let fish = Object.create(Animal);
fish.type = 'Fishes';
fish.displayType();           // Output:Fishes
```

see [[#Prototype chain]]

# `this` object

JavaScript has a special keyword, `this`, that you can use within a **method** to refer to the current object.

```js
const Manager = {
  name: "John",
  age: 27,
  job: "Software Engineer"
}
const Intern = {
  name: "Ben",
  age: 21,
  job: "Software Engineer Intern"
}

function sayHi() {
    console.log('Hello, my name is', this.name)
}

// add sayHi function to both objects
Manager.sayHi = sayHi;
Intern.sayHi = sayHi;

Manager.sayHi() // 'Hello, my name is John'
Intern.sayHi()  //'' Hello, my name is Ben'
```


# Prototype chain

[Inheritance and the prototype chain - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Inheritance_and_the_prototype_chain)


> [!NOTE] 
> JavaScript 是动态的，本身不提供一个 class 的实现。即便是在 ES2015/ES6 中引入了 class 关键字，但那也只是语法糖，JavaScript 仍然是基于原型的。
>当谈到继承时，JavaScript 只有一种结构：对象。每个实例对象（object）都有一个私有属性（称之为 `__proto__` ）指向它的构造函数的原型对象（prototype）。该原型对象也有一个自己的原型对象（`__proto__`），层层向上直到一个对象的原型对象为 `null`。根据定义，`null` 没有原型，并作为这个原型链中的最后一个环节.

Each object has a private property which holds a link to another object called its **prototype**.

    obj ---> obj.prototype ---> ... ---> Object ---> null

Nearly all objects in JavaScript are instances of `Object` which sits on the top of a prototype chain. By definition, `null` has no prototype, and acts as the final link in this **prototype chain**.

> When trying to access a property of an object, the property will not only be sought on the object but on the prototype of the object, the prototype of the prototype, and so on until either a property with a matching name is found or the end of the prototype chain is reached.

```js
// getter and setter of obj's prototype
Object.getPrototypeOf(obj);    
Object.setPrototypeOf(obj);
```

> [!WARNING]
> It should not be confused with the `func.prototype` property of **functions**, which instead specifies the `Prototype` to be assigned to all _instances_ of objects created by the given function when used as a constructor.

## Inheritance

Setting a property to an object creates an own property. The only exception to the getting and setting behaviour rules is when there is an inherited property with a `getter` or a `setter`.

```js

let F_proto = {a: 1, b: 2};
let F = function(c, d) {
  this.c = c;
  this.d = d;
}
F.prototype = F_proto;

let o = new F(3, 4);

console.log(o.a); // refer to o.__proto__.a
```

When an inherited function is executed, the value of this points to the inheriting object, not to the prototype object where the function is an own property.

    methods are virtual

```js
var o = {
  a: 2,
  m: function() {
    return this.a + 1;
  }
};

console.log(o.m()); // 3
// When calling o.m in this case, 'this' refers to o

var p = Object.create(o);
// p is an object that inherits from o

p.a = 4; // creates a property 'a' on p
console.log(p.m()); // 5
// when p.m is called, 'this' refers to p.
// So when p inherits the function m of o,
// 'this.a' means p.a, the property 'a' of p
```
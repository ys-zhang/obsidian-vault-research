vanilla js scripts do not support module, there are a number of JavaScript libraries and frameworks that enable module usage (for example, other [[CommonJS]] and AMD-based module systems like [[RequireJS]], and more recently [[Webpack]] and [[Babel]]).


# `.mjs` V.S. `.js`

[[V8]] recommend `.mjs` extension name for js module files.

- It is good for clarity, i.e. it makes it clear which files are modules, and which are regular JavaScript.
- It ensures that your module files are parsed as a module by runtimes such as [[Node.js]], and build tools such as [[Babel]].

> To get modules to work correctly in a browser, you need to make sure that your server is serving them with a `Content-Type` header that contains a JavaScript [[MIME type]] such as `text/javascript`. If you don't, you'll get a strict MIME type checking error along the lines of "The server responded with a non-JavaScript MIME type" and the browser won't run your JavaScript.

> Most servers already set the correct type for `.js` files, but not yet for `.mjs` files. Servers that already serve `.mjs` files correctly include [GitHub Pages](https://pages.github.com/) and [`http-server`](https://github.com/http-party/http-server#readme) for Node.js.

> The `<script type="module">` attribute is used to denote when a module is being pointed to.

# Browser support 

[JavaScript modules - JavaScript | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)

[ES6 In Depth: Modules - Mozilla Hacks - the Web developer blog](https://hacks.mozilla.org/2015/08/es6-in-depth-modules/)

modern browsers have started to support module functionality natively,

Use of native JavaScript modules is dependent on the `import` and `export` statements.


## export

```javascript

export const name = 'square';  // type one

export { name, draw, reportArea, reportPerimeter }; // type two
```

There is also a type of export called the default export — this is designed to make it easy to have a default function provided by a module, and also helps JavaScript modules to interoperate with existing [[CommonJS]] module systems (as explained nicely in ES6 In Depth: Modules by Jason Orendorff; search for "Default exports").

```js
export * from 'x.js';
export { name } from 'x.js';
```

```js
// fetch request
const colors = fetch('../data/colors.json')
	.then(response => response.json());

export default await colors;

```

## import

```javascript

// from 'a path relative to the site root'
import { name, draw, reportArea, reportPerimeter } from './modules/square.js';
// Creating a module object
import * as Module from './modules/module.js';

import('./modules/myModule.js')
  .then((module) => {
    // Do something with the module.
  });


squareBtn.addEventListener('click', () => {
  import('./modules/square.js').then((Module) => {
    let square1 = new Module.Square(myCanvas.ctx, myCanvas.listId, 50, 50, 100, 'blue');
    square1.draw();
    square1.reportArea();
    square1.reportPerimeter();
  })
});
```


## Applying the module to  HTML

to declare this script as a module. To import the `main.js` script, we use this:
```html
<script type="module" src="main.js"></script>
```

You can also embed the module's script directly into the HTML file by placing the JavaScript code within the body of the `<script>` element:

```html
<script type="module">
  /* JavaScript module code here */
</script>
```

> You can only use `import` and `export` statements inside modules, not regular scripts.


-   You need to pay attention to local testing — if you try to load the HTML file locally (i.e. with a `file://` URL), you'll run into CORS errors due to JavaScript module security requirements. You need to do your testing through a server.
-   There is no need to use the defer attribute (see `<script>` attributes) when loading a module script; *modules are deferred automatically.*
-   Modules are only executed once, even if they have been referenced in multiple `<script>` tags.
-   Last but not least, let's make this clear — module features are imported into the scope of a single script — they aren't available in the global scope.


## role of transpile and bundling tools

But now we get to the fun part of this system. There’s a cool trick. Because the system doesn’t specify how loading works, and because you can figure out all the dependencies ahead of time by looking at the import declarations in the source code, an implementation of ES6 is free to do all the work at compile time and bundle all your modules into a single file to ship them over the network! And tools like [[Webpack]] actually do this.

This is a big deal, because loading scripts over the network takes time, and every time you fetch one, you may find that it contains import declarations that require you to load dozens more. A naive loader would require a lot of network round trips. But with [[Webpack]], not only can you use ES6 with modules today, you get all the software engineering benefits with no run-time performance hit.

A detailed specification of module loading in ES6 was originally planned—and built. One reason it isn’t in the final standard is that there wasn’t consensus on how to achieve this bundling feature. I hope someone figures it out, because as we’ll see, module loading really should be standardized. And bundling is too good to give up.


# server side [[CommonJS]]

from [CommonJS JavaScript Module Format, an introduction](https://flaviocopes.com/commonjs/)

## import 

```js
const package = require('module-name')

const { a, b, c } = require('./uppercase.js')
```

In CommonJS, modules are loaded synchronously, and processed in the order the JavaScript runtime finds them. This system was born with server-side JavaScript in mind, and is not suitable for the client-side (this is why ES Modules were introduced).

## export

```js
exports.a = 1
exports.b = 2
exports.c = 3
```

### just export one value

```js
//file.js
module.exports = value


// import 
const value = require('./file.js')
```


# [[Node.js]] module

see [[Node.js#Module]]
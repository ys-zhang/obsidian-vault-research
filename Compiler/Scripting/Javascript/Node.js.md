# Module
[Modules: CommonJS modules](https://nodejs.org/api/modules.html#modules_modules_commonjs_modules)

Node.js adopts [[CommonJS]]'s specification.

	In the Node.js module system, each file is treated as a separate module.
	
```js

// import
// file: main.js
const circle = require('./circle.js');
console.log(`The area of a circle of radius 4 is ${circle.area(4)}`);

// export
// file: ./circle.js

const { PI } = Math;

exports.area = (r) => PI * r**2;
exports.circumference = (r) => 2 * PI * r;

// default export 
// file: ./square.js
module.exports = class Square {
	...
};
```

## if __name__ == "__main__" in python

`require.main` is the `module` object of the entry point.

That means that it is possible to determine whether a file has been run directly by testing `require.main === module`.

```js

if (require.main === module) {
	// do sth
}

/*
equiv to py

if __name__ == "__main__":
	# do sth
	pass
*/
```


## require path

```
require('foo')

/
|---- node_modules 
          |------- foo  (symlink to) ------> /usr/lib/node/foo/ver/..
```

## types of module

### File modules

If the exact filename is not found, then Node.js will attempt to load the required filename with the added extensions: `.js`, `.json`, and finally `.node`.

`.js` files are interpreted as JavaScript text files, and `.json` files are parsed as JSON text files. `.node` files are interpreted as compiled addon modules loaded with `process.dlopen()`.

### Folders as modules

The first is to create a [`package.json`](https://nodejs.org/api/packages.html#packages_node_js_package_json_field_definitions) file in the root of the folder, which specifies a `main` module. An example [`package.json`](https://nodejs.org/api/packages.html#packages_node_js_package_json_field_definitions) file might look like this:

```json
{ 
  "name" : "some-library",
  "main" : "./lib/some-library.js" // bydefault "main" is "index.js"
}  
```

If this was in a folder at `./some-library`, then `require('./some-library')` would attempt to load `./some-library/lib/some-library.js`.

### Loading from `node_modules` folders

If the module identifier passed to `require()` is not a core module, and does not begin with `'/'`, `'../'`, or `'./'`, then Node.js starts at the parent directory of the current module, and adds `/node_modules`, and attempts to load the module from that location. Node.js will not append `node_modules` to a path already ending in `node_modules`.

If it is not found there, then it moves to the parent directory, and so on, until the root of the file system is reached.

For example, if the file at `'/home/ry/projects/foo.js'` called `require('bar.js')`, then Node.js would look in the following locations, in this order:

-   `/home/ry/projects/node_modules/bar.js`
-   `/home/ry/node_modules/bar.js`
-   `/home/node_modules/bar.js`
-   `/node_modules/bar.js`

This allows programs to localize their dependencies, so that they do not clash.

It is possible to require specific files or sub modules distributed with a module by including a path suffix after the module name. For instance `require('example-module/path/to/file')` would resolve `path/to/file` relative to where `example-module` is located. The suffixed path follows the same module resolution semantics.

### Loading from the global folders

If the `NODE_PATH` environment variable is set to a colon-delimited list of absolute paths, then Node.js will search those paths for modules if they are not found elsewhere.

## The module wrapper

Before a module's code is executed, Node.js will wrap it with a function wrapper that looks like the following:

```js
(function(exports, require, module, __filename, __dirname) {
// Module code actually lives in here
});
```

By doing this, Node.js achieves a few things:

-   It keeps top-level variables (defined with `var`, `const` or `let`) scoped to the module rather than the global object.
-   It helps to provide some global-looking variables that are actually specific to the module, such as:
    -   The `module` and `exports` objects that the implementor can use to export values from the module.
    -   The convenience variables `__filename` and `__dirname`, containing the module's absolute filename and directory path.



## Altogether of what `require` does
Putting together all of the above, here is the high-level algorithm in pseudocode of what `require()` does:

```
require(X) from module at path Y
1. If X is a core module,
   a. return the core module
   b. STOP
2. If X begins with '/'
   a. set Y to be the filesystem root
3. If X begins with './' or '/' or '../'
   a. LOAD_AS_FILE(Y + X)
   b. LOAD_AS_DIRECTORY(Y + X)
   c. THROW "not found"
4. If X begins with '#'
   a. LOAD_PACKAGE_IMPORTS(X, dirname(Y))
5. LOAD_PACKAGE_SELF(X, dirname(Y))
6. LOAD_NODE_MODULES(X, dirname(Y))
7. THROW "not found"

LOAD_AS_FILE(X)
1. If X is a file, load X as its file extension format. STOP
2. If X.js is a file, load X.js as JavaScript text. STOP
3. If X.json is a file, parse X.json to a JavaScript Object. STOP
4. If X.node is a file, load X.node as binary addon. STOP

LOAD_INDEX(X)
1. If X/index.js is a file, load X/index.js as JavaScript text. STOP
2. If X/index.json is a file, parse X/index.json to a JavaScript object. STOP
3. If X/index.node is a file, load X/index.node as binary addon. STOP

LOAD_AS_DIRECTORY(X)
1. If X/package.json is a file,
   a. Parse X/package.json, and look for "main" field.
   b. If "main" is a falsy value, GOTO 2.
   c. let M = X + (json main field)
   d. LOAD_AS_FILE(M)
   e. LOAD_INDEX(M)
   f. LOAD_INDEX(X) DEPRECATED
   g. THROW "not found"
2. LOAD_INDEX(X)

LOAD_NODE_MODULES(X, START)
1. let DIRS = NODE_MODULES_PATHS(START)
2. for each DIR in DIRS:
   a. LOAD_PACKAGE_EXPORTS(X, DIR)
   b. LOAD_AS_FILE(DIR/X)
   c. LOAD_AS_DIRECTORY(DIR/X)

NODE_MODULES_PATHS(START)
1. let PARTS = path split(START)
2. let I = count of PARTS - 1
3. let DIRS = [GLOBAL_FOLDERS]
4. while I >= 0,
   a. if PARTS[I] = "node_modules" CONTINUE
   b. DIR = path join(PARTS[0 .. I] + "node_modules")
   c. DIRS = DIRS + DIR
   d. let I = I - 1
5. return DIRS

LOAD_PACKAGE_IMPORTS(X, DIR)
1. Find the closest package scope SCOPE to DIR.
2. If no scope was found, return.
3. If the SCOPE/package.json "imports" is null or undefined, return.
4. let MATCH = PACKAGE_IMPORTS_RESOLVE(X, pathToFileURL(SCOPE),
  ["node", "require"]) defined in the ESM resolver.
5. RESOLVE_ESM_MATCH(MATCH).

LOAD_PACKAGE_EXPORTS(X, DIR)
1. Try to interpret X as a combination of NAME and SUBPATH where the name
   may have a @scope/ prefix and the subpath begins with a slash (`/`).
2. If X does not match this pattern or DIR/NAME/package.json is not a file,
   return.
3. Parse DIR/NAME/package.json, and look for "exports" field.
4. If "exports" is null or undefined, return.
5. let MATCH = PACKAGE_EXPORTS_RESOLVE(pathToFileURL(DIR/NAME), "." + SUBPATH,
   `package.json` "exports", ["node", "require"]) defined in the ESM resolver.
6. RESOLVE_ESM_MATCH(MATCH)

LOAD_PACKAGE_SELF(X, DIR)
1. Find the closest package scope SCOPE to DIR.
2. If no scope was found, return.
3. If the SCOPE/package.json "exports" is null or undefined, return.
4. If the SCOPE/package.json "name" is not the first segment of X, return.
5. let MATCH = PACKAGE_EXPORTS_RESOLVE(pathToFileURL(SCOPE),
   "." + X.slice("name".length), `package.json` "exports", ["node", "require"])
   defined in the ESM resolver.
6. RESOLVE_ESM_MATCH(MATCH)

RESOLVE_ESM_MATCH(MATCH)
1. let { RESOLVED, EXACT } = MATCH
2. let RESOLVED_PATH = fileURLToPath(RESOLVED)
3. If EXACT is true,
   a. If the file at RESOLVED_PATH exists, load RESOLVED_PATH as its extension
      format. STOP
4. Otherwise, if EXACT is false,
   a. LOAD_AS_FILE(RESOLVED_PATH)
   b. LOAD_AS_DIRECTORY(RESOLVED_PATH)
5. THROW "not found"
```
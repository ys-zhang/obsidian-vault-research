#ui  #React  #finite-state-machine  


This is a reading note of [Intro to React, Redux and TypeScript](https://blog.isquaredsoftware.com/2020/12/presentations-react-redux-ts-intro/) by Mark Erikson, the author of [redux tools](https://redux.js.org/)

# React

React + Redux is based on the idea that what's really hard with writing web apps is _non-deterministic behaviour and unclear data flow_.

> React encourages you to think of your app and UI in terms of _state_ rather than explicit UI manipulation.


Components in react are like _state machine_ and functions

```haskell
class Component f where 
type State f
ui :: f -> State f -> UI

```

1. A React function component will be called by React. This proc is called **Rendering**.
2. when a component renders, it returns a tree of _react component descriptions_, which will eventually be turned into DOM nodes.
3. `ReactDOM.render(component, parentNode)` creates a new component tree and appends the generated DOM output from the component and its children to the parent DOM node.

```jsx
import React from 'react';
import ReactDOM from 'react-dom';

function HelloWorld() {
  /*
     the jsx is just a syntax suger for 
     React.createElement("div", null, "Hello World!");
   */
  return <div> Hello World! </div>
}

const parentNode = document.getElementById("root");
ReactDOM.render(<HelloWorld/>, parentNode);
```
React Element Objects look like `{type: ComponentType, props: {}, children: d[]}`

```ts
type ReactElement {
  type: string,
  props: object,
  chldren: ReactElement[],
  attributes : object,
}
```


# Redux

Principles of Redux:

1. _Single source of truth_
2. _State is readonly_
3. _Changes are made with pure functions_






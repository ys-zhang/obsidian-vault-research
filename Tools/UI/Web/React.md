# React Elements

```jsx
const elem = <h1 className='greeting'> Hello, world!</h1>;
```
will be compiled to 
```js
const element = React.createElement(
  'h1',
  {className: 'greeting'},
  'Hello, world!'
);
```

React elements are [immutable](https://en.wikipedia.org/wiki/Immutable_object). 
Once you create an element, you can’t change its children or attributes. 
An element is like _a single frame in a movie_: it represents the UI at a certain point in time.

```jsx
const root = ReactDOM.createRoot(
  document.getElementById('root')
);
const element = <h1>Hello, world</h1>;
root.render(element);
```

# React Component

> Conceptually, components are like JavaScript functions. 
> They accept arbitrary inputs (called “props”) and return **React elements** describing what should appear on the screen.

>[!WARNING] Component Rule 
> All React components must act like _pure_ functions with respect to their props.


```jsx
function Welcome(props) {  
  return <h1>Hello, {props.name}</h1>;
}

const root = ReactDOM.createRoot(document.getElementById('root'));
const element = <Welcome name="Sara" />;
root.render(element);
```

1.  We call `root.render()` with the `<Welcome name="Sara" />` element.
2.  React calls the `Welcome` component with `{name: 'Sara'}` as the props.
3.  Our `Welcome` component returns a `<h1>Hello, Sara</h1>` element as the result.
4.  React DOM efficiently updates the DOM to match `<h1>Hello, Sara</h1>`.

## States

> State is similar to props, but it is _private_ and _fully controlled_ by the component.

```jsx
class Clock extends React.Component {

  constructor( props ) {
    super( props );
    this.state = { date: new Date };
  }

  componentDidMount() {
    this.timerID = setInterval(
      () => this.tick(),
      1000
    );
  }

  componentWillUnmount() {
    clearInterval(this.timerID);
  }

  tick() {
    this.setState({
      date: new Date()
    });
  }  
  
  render() {
    return <p> It is {this.state.date.toLocaleTimeString()} now. </p>; 
  }
  
}
```


>[!WARNING]
> 1. use `this.setState` instead of redirect update or assignment
> 2. Because `this.props` and `this.state` may be updated _asynchronously_, you **should not rely on their values for calculating the next state**. Instead, use the function input form of `setState`
> 3. state merging is **shallow**, i.e., only happens at the first level.


## Misc.

1. component returns `null` will not be rendered;
2. `true && expr` evaluates to `expr` and `false && expr` evaluates to `false`. 


# Form and Controlled Components

An _input form element_ whose value is _controlled by React_ is called a “controlled component”.


# Hooks

https://www.swyx.io/hooks/


# React Render Behaviour

[origin blog](https://blog.isquaredsoftware.com/2020/05/blogged-answers-a-mostly-complete-guide-to-react-rendering-behavior/)

## Render Process

> **Rendering** is the process of React asking your components to describe what they want their section of the UI to look like, now, based on the current combination of props and state.


<iframe src="https://projects.wojtekmaj.pl/react-lifecycle-methods-diagram/"  style="width:100%; height: 670px" />

1. traverse the component tree and filter out components that needs to be updated.
2. **_Render_**: call `classComp.render()`  or `funcComp()`  
3. save the rendered output
4. **_Reconciliation_**:  diff the new tree of react elements and collect those need to be applied to real DOM.
5. **_Commit_**:  applies all the calculated changes to the DOM in one __synchronous__ sequence.
6. update `refs` created by `useRef`
7. **synchronously** runs:
    - Class Component:  `componentDidMount`,  `componentDidUpdate`
    - Function Component:
        1. `useLayoutEffect`
        2. **[[#Passive Effects]]**: sets a short timeout, and when it expires, runs all the `useEffect` hooks.

> [!WARNING]
> "rendering" is not the same thing as "updating the DOM", and a component may be rendered without any visible changes happening as a result.

When React renders a component:
-   The component might return the same render output as last time, so no changes are needed
-   In _Concurrent Mode_, React might end up rendering a component multiple times, but throw away the render output each time if other updates invalidate the current work being done


**Rendering a component will, by default, cause _all_components inside of it to be rendered too!**

**In normal rendering, React does _not_ care whether "props changed" - it will render child components unconditionally just because the parent rendered!**

**rendering is not a _bad_ thing - it's how React knows whether it needs to actually make any changes to the DOM!**

**rendering must be "pure" and not have any side effects!**


## Fibre

`Fibre` tracks all current component instances, and stores:
1. component type
2. props
3. state
4. pointers to parent, sibling and children
5. [linked list of hooks](https://www.swyx.io/hooks/#not-magic-just-arrays)


## Passive Effects


# FAQ

- [use effect called twice in dev mode](https://stackoverflow.com/questions/60618844/react-hooks-useeffect-is-called-twice-even-if-an-empty-array-is-used-as-an-ar)


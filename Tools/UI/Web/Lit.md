# Caveats

1. context consumer (with `{ ...,  subscribe: true}` ) will trigger an update whenever the context changes, even when the render function do not access to the context;
2. signals can solve the above problem, remember to unset `subscribe`;
3. context can be a primitive value;
4. Lit have an identity check for the returned value of `render()`, the DOM is only update when the check failed. 
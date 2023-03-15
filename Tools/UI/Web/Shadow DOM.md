#html #web 


From [MDN Doc](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_shadow_DOM)

_Shadow_ DOM allows _hidden DOM trees_ to be attached to elements in the regular DOM tree — this shadow DOM tree starts with a _shadow root_, underneath which you can attach any element, in the same way as the normal DOM.
![](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_shadow_DOM/shadowdom.svg)
- **Shadow host**: The regular DOM node that the shadow DOM is attached to.
- **Shadow tree**: The DOM tree inside the shadow DOM.
- **Shadow boundary**: the place where the shadow DOM ends, and the regular DOM begins.
- **Shadow root**: The root node of the shadow tree.

> none of the code inside a shadow DOM can affect anything outside it, allowing for handy encapsulation.



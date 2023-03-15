#web #javascript #css-tricks

From the blog [Focusing on Focus](https://blog.whatwg.org/focusing-on-focus)

# Type of Focus

- **Programmatically Focusable**: get focused when calling the `focus()` method on it or when putting an `autofocus` attribute on the element. In all platforms, all elements that are either _click focusable_ or _sequentially focusable_ are also _programmatically focusable_.
- **Click Focusable**: get focused when it’s clicked. This has the same set of elements as _programmatically focusable_ in most UAs/platforms. A notable _exception_ is Safari where non-editable form controls (`checkboxes`, etc.) are not _click focusable_ by default.
- **Sequentially Focusable**: be focused through _“tabbing”_

>[!danger] 
> `visual: hidden`  and `visual: collapse` elements are always **unfocusable**


# Related Attributes

## The `tabindex` Attribute

```js
/// property
Element.tabIndex
```
In the spec, the `js` property will be `n` if the attribute is explicitly set `<some-elem tabindex="n">...</some-elim>`, otherwise:
- `0` if the element is _focusable by default_
- `-1` if the element is _not focusable by default_

_However the default value of the `tabIndex` property is not implemented in any main stream browser._

> You can also modify the order of elements traversed with sequential navigation/tabbing — elements with a positive `tabindex` value will be traversed first, in _ascending order_ (and in tree order in case of a tie), and then elements with a `tabindex` value of zero (or unspecified but the element is sequentially focusable by default), in tree order.

>[!NOTE] Default Focusable Element
> You can’t, however, make an element not focusable at all through this attribute — there is no value you can set the `tabindex` attribute to on a `<button>` that will stop it from being focusable.   


## The `autofocus` attribute

The `autofocus` content attribute is useful if you want to set focus on a _form_ control element on _page load_.

### Caveats

- `autofocus` is not accessible in most controls other than form elements;
- `autofocus` is will not function if the container page is accessed by _url with fragments_.
-  `autofocus` set on multiple elements is an undefined behaviour.

## `inert` attribute

[The `inert` attribute](https://whatpr.org/html/4288/interaction.html#the-inert-attribute) is a [global HTML attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes) that makes the browser _"ignore"_ user input events for the element, including _focus_ events and events from assistive technologies. The browser may also ignore _page search_ and _text selection_ in the element.

```html
<body>
  <div 
    aria-labelledby="modal-title"
    class="c-modal" 
    id="modal" 
    role="dialog" 
    tabindex="-1">
    <div role="document">
      <h2 id="modal-title">Save changes?</h2>
      <p>The changes you have made will be lost if you do not save them.<p>
      <button type="button">Save</button>
      <button type="button">Discard</button>
    </div>
  </div>
  <main inert>
    <!-- ... -->
  </main>
</body>
```

## `:focus-within` pseudo-class

The **`:focus-within`** pseudo class matches an element if the _element or any of its descendants_ are focused.

<iframe height="300" style="width: 100%;" scrolling="no" title=":focus-within" src="https://codepen.io/geoffgraham/embed/gewWjN?default-tab=html%2Cresult" frameborder="no" loading="lazy" allowtransparency="true" allowfullscreen="true"/>

# Tricks

## Focus Trapping

Requirements
- `tab/shift-tab` looping inside trapping element

Methods
- https://css-tricks.com/a-css-approach-to-trap-focus-inside-of-an-element/
- https://css-tricks.com/focus-management-and-inert/
- https://dev.to/r3l_k3r/focus-trappinglooping-for-keyboard-navigation-l6m

#web 
#CSS

# Sizing


![[CSS-box-model.excalidraw|100%]]

>[!NOTE]
>HTML Elements have a _natural/intrinsic size_, set before they are affected by any CSS.
>
>When we size an element we are giving it an _extrinsic size_, i.e, it will have the size _regardless_ of its content. (we may have the problem of _overflow_).

>[!WARNING]
>fixing the height of elements with lengths or percentages is something we need to do very carefully on the web

> [!NOTE]
> When you use margin and padding set _in percentages_, the value is calculated from the **inline size** of the containing block — therefore the _width_ when working in a horizontal language.

## width

![[Pasted image 20230127142124.png]]

## Sizing a `div`

`div`'s _intrinsic size_ is determined by its contents, a empty `div` will have a $0$ height but stretches to the width of its container, since its a _block element_.  

# Position

| position type      | document flow       | relative                                                                                | misc                                                                                                     |
| ------------------ | ------------------- | --------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| `static` (default) | normal flow         | take no effect                                                                          | `z-index` have no effect                                                                                 |
| `relative`         | normal flow         | offset w.r.t. normal flow position                                                      | act as a position reference for _absolutely positioned children_                                         |
| `absolute`         | outside normal flow | offset w.r.t. to the _nearest parent_'s _edges_ that has a position other than `static` |                                                                                                          |
| `fixed`            | outside normal flow | offset w.r.t. to the [[viewport]]                                                         |                                                                                                          |
| `sticky`           | normal flow         |                                                                                         | `relative` until it crosses a specified threshold, then treat it as fixed until its parent is off screen | 

- An element is **positioned** if it is not `position: static`
- `z-index` can only take effect on a **positioned** element (element not `static`)


# FAQ

## aspect ratio

```html
<iframe width="100%" style="aspect-ratio: <width: int> / <height: int>" src=.../>
```


# Tricks

## Focus Trapping

![[Tools/UI/Web/Focus#Tricks]]


## Input Suggestion and Completion (fish-shell like)

https://stackoverflow.com/questions/4663710/how-do-i-implement-autocomplete-without-using-a-dropdownlist

```html
<div id="search-container">
    <input type="text" name="search" id="search" />
    <input type="text" disabled="disabled" id="suggestion" />
</div>
```

```css
#container {
    position: relative;
}

#search {
    position: relative;
    color: #000;
    z-index: 10;
    background: transparent;
    // border, etc....
}

#suggestion {
    position: absolute;
    top: 0;
    left: 0;
    z-index: 0;
    color: #ccc;
    border: none;
    // ...
}
```

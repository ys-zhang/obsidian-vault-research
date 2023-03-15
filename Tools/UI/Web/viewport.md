#CSS #web #html 

https://developer.mozilla.org/en-US/docs/Web/CSS/Viewport_concepts

A **viewport** represents the area in computer graphics being currently viewed.
It is generally the same as the browser window, _excluding_ the UI, menu bar, etc.

N.B.
- viewport sizes are mutable
- The web contains two viewports, the **layout viewport** and the **visual viewport**. 

When the user pinch-zooms the page, pops open a dynamic keyboard, or when a previously hidden address bar becomes visible, _the visual viewport shrinks but the layout viewport is unchanged_.

| width                                  | semantics                                        |
| -------------------------------------- | ------------------------------------------------ |
| `document.documentElement.clientWidth` | viewport width (exclude border,margin,scrollbar) |
| `window.innerWidth`                    | browser inner width (include scrollbar)          |
| `windodw.outerWidth`                   | Chrome app's width                                                 |

# Visual Viewport

The **visual viewport** is the part of the web page that is _currently visible_ in the browser and _can change_.





# Layout Viewport

The area within the `innerHeight` and `innerWidth` is generally considered the **layout viewport**.

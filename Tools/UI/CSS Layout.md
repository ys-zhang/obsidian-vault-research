[CSS layout - Learn web development | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Learn/CSS/CSS_layout)

# Frequently Used Attributes

## display

[display - CSS: Cascading Style Sheets | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/CSS/display)

Formally, the **`display`** property sets an element's **inner** and **outer** _display types_.
- the inner type sets the layout of children
- The outer type sets an element's participation in [[flow layout]], which can be 
    - _inline element_
    - _block element_
    
    for differences see [Inline elements - HTML: HyperText Markup Language | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements)
- Grid layout:
    - `display: grid`
    - `display: inline-grid`


# Measure Units

1. `px` 
    
    stands for pixel;
2. `pt` stands for point;
    
    A point (`pt`) is a measurement of space that is **independent** on screen resolution. For instance, in `@1x` resolution, `1pt = 1px`, in `@2x` resolution, `1pt = 2px`.  
  ![[Pasted image 20221001144929.png]]
3. `em` & `rem` 
    
    represents a calculated length from the current element's font-size and root's font-size respectively.
4. `ex`
    
    represents length of current elements font-size's x-height


# Flow Layout (流式布局)

[CSS Flow Layout - CSS: Cascading Style Sheets | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout)

TLDR;

1. _Inline elements_ display one after the other, starting on the left;
2. _Block elements_ start at the top and move down the page.


# Flex Layout


# Grid Layout (网格布局)
[CSS Grid Layout - CSS: Cascading Style Sheets | MDN (mozilla.org)](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout)

> [!NOTE]
> The basic difference between CSS Grid Layout and [CSS Flexbox Layout](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout) is that flexbox was designed for layout in one dimension - either a row or a column. Grid was designed for two-dimensional layout - rows, and columns at the same time.
>
> - Flexbox works from the **content out**. An ideal use case for flexbox is when you have a set of items and want to space them out evenly in a container. _You let the size of the content decide how much individual space each item takes up._
> - Grid works from the **layout in**. When you use CSS Grid Layout you create a layout and then you place items into it, or you allow the auto-placement rules to place the items into the grid cells according to that strict grid. _It is possible to create tracks that respond to the size of the content, however, they will also change the entire track._



## Concepts

- A _grid track_ is the space between any two lines on the grid.
    - `grid-template-columns: 200px 200px 200px`: 3 column defined in the grid layout.
    - `grid-template-rows: repeat(3, 1fr)`: 3 rows, each with width `1fr`, see [repeat()](https://developer.mozilla.org/en-US/docs/Web/CSS/repeat). 
- The `fr` unit represents a _fraction of the available space_ in the grid container.
    - if `fr` and `px` are both used in the same grid container, then fixed width is take out from the available space then `fr`'s value is computed.
- _Grid Lines_ are numbered (start from $1$) according to the writing mode of the document.
    - In a left-to-right language, line 1 is on the left-hand side of the grid.
    - In a right-to-left language, it is on the right-hand side of the grid.
    - Lines can also be named.
- Place _Grid items_ using  _Grid Lines_ :
    - `grid-column-start`, `grid-column-end`
    - `grid-row-start`, `grid-row-end`
    - `grid-column: b / e` equivalent to `grid-column-start: b` and `grid-column-end: e` 
    - `grid-row: b / e` equivalent to `grid-row-start: b` and `grid-row-end: e` 
    - `grid-column: a` equivalent to `grid-column: a / a+1`
- A _grid cell_ is the smallest unit on a grid. Conceptually it is like a table cell.
- _Grid items_ can span one or more cells both by row or by column, and this creates a _grid area_.
- _Gutters_ or _alleys_ (spacing) between grid cells can be created using the [`column-gap`](https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap) and [`row-gap`](https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap) properties, or the shorthand [`gap`](https://developer.mozilla.org/en-US/docs/Web/CSS/gap).
- use `z-index` to control _overlapping_ grid items.


# 8-Point Grid

[Original blog](https://spec.fm/specifics/8-pt-grid)

>[!NOTE] The Basic Principles
> 1. _Dimensions_, _padding_ and _margin_ of both _block_ and _inline_ elements are all `8x` i.e., multiply of $8$;
> 2. When the **only** contents of a _block_ are _text_, set the text-size consistent with the rest of the UI, then use _padding_ to determine the size of the _container_ block

## Tips

1. Rems and Variables
    
    If you set your root text size to $16$, you can easily use .$5rem$ increments to build your layouts on an 8-point grid.
2. Frame your icons
    
    Putting a frame around an icon is a simple way to keep measurements consistent.


## The Hard Grid Approach

**Hard grid** involves placing objects on a fixed grid with 8-point increments. In Figma, this would involve applying a uniform grid to the frame with a size of $8$.

![[Pasted image 20221001160412.png]]

The primary argument for the _Hard Grid method_ is that by using additional transparent background elements and then grouping them to small groups of foreground elements, _you can keep track of margin and padding on a per-element basis_ and just snap these containers to the grid like bricks.


## The Soft Grid Approach

**Soft grid** involves placing objects at distances from each other that are divisible by $8$. This would involve applying a row or column layout grid with properties divisible by $8$.

![[Pasted image 20221001160442.png]]

The argument for the _Soft Grid method_ is that when it comes time to code up an interface, using an actual grid is irrelevant because programming languages don’t use that kind of grid structure - it’ll just get thrown away. When the speed at which you arrive at a high-quality, programmable set of mockups is a priority, bypassing Hard Grid’s extra overhead of managing additional layers in favour of Soft Grid’s more fluid, minimal structure can be an advantage. This also can be more favourable to iOS where many system UI elements are not defined by an even grid.



#CSS 

> CSS Anchor Positioning gives us a simple interface to attach elements next to others just by saying which sides to connect — directly in CSS. It also lets us set a fallback position so that we can avoid the overflow issues.

>[!def] anchor
> This is the element used as a reference for positioning other elements, hence the _anchor_ name.

>[!def] target
> This is an absolutely positioned element placed relative to one or more anchors. The _target_ is the name we will use from now on, but you will often find it as just an “absolutely positioned element” in the spec.

>[!def] containing block
> This is the box that contains the elements. 
> For an absolute element, the containing block is the viewport the closest ancestor with a position other than `static` or certain values in properties like [`contain`](https://css-tricks.com/almanac/properties/c/contain/) or [`filter`](https://css-tricks.com/almanac/properties/f/filter/).

>[!def] IMCB
> For an absolute element, inset properties (`top`, `right`, `bottom`, `left`, etc.) reduce the size of the containing block into which it is sized and positioned, resulting in a new box called the _inset-modified containing block_, or IMCB for short.


# 1 associate anchor and target

- (css method)
- (html attribute method^[not fully supported])

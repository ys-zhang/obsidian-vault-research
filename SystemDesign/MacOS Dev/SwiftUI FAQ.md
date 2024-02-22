1. what is `containerShape`?
2. how `frame` works?
3. how `containerRelativeFrame` works?
4. `overlay` VS `ZStack`?
5. font char width to `CGSize`?
6. `GeometryReader`?
7. `coordinateSpace(_:)`


# Concurrency

actor guarantees that only one task can access actor fields and methods at a time. One pitfall is that this guarantee does not across suspension points (i.e., `await`). The real semantic model is that, with in actor methods:
- acquiring the lock at 
  1. just before executing any code
  2. after any suspension finished, i.e. resume from `await`
- release the lock at 
  1. just before return from the method
  2. just before suspend, i.e. entering `await`

```swift
actor SomeActor { 
  func someMethod() async -> ... { 
    acquire_lock()
    ....
    release_lock() 

    await someTask   // suspension point

    acquire_lock() 
    ... 
    release_lock() 

    await someTake   // suspension point

    acquire_lock() 
    ... 
    release_lock() 
    return ...
  }
}
```

# Layout

## Fundamental Algorithm

1. A _parent_ view proposes a size for its child.
2. Based on that information, the _child_ then chooses its own size ([`sizeThatFits`](https://developer.apple.com/documentation/swiftui/nsviewrepresentable/sizethatfits(_:nsview:context:)-2lqoz)) and the parent _must_ respect that choice.
3. The parent then positions the child in its _coordinate space_.


### Positioning

SwiftUI gives us two ways of positioning views: absolute positions using `position()`, and relative positions using `offset()`.

## Pitfalls

- if your view hierarchy is wholly _layout neutral_, then it will automatically take up all available space.
- when we use `position()` we get back _a new view that takes up all available space_, so it can position its child  at the correct location.
- When we use the `offset()` modifier, we’re changing the location where a view should be rendered without actually changing its underlying geometry.

## Other interesting facts:

- `Color` is a `View` which is _layout neutral_
- There are some [User Interface Queries/Tests](https://developer.apple.com/documentation/xctest/user_interface_tests) can be used to retrieve info about current device settings about UI. For instance `horizontalSizeClass`
- By default `ZStack` drawn first element first, then subsequent views are layered over it. 
- Use `.zIndex()` to programatically change element's layering.
- The `.zIndex()` modifier only affects drawing order _inside_ the current `ZStack`, so if you have two overlapping stacks you need to think about the Z index of the stacks as well as the views _inside_ the stacks.
- `ScrollView` automatically sizes itself to fit the content we place inside it and also automatically adds extra insets to avoid the _safe area_.
- `ScrollView` do not support Zooming
### Example

```swift
struct ContentView: View {
    var body: some View {
        Text("Hello, World!")
            .padding(20)
            .background(.red)
    }
}
```
    
>[!Note]
>- (1) SwiftUI: You can have the whole screen, how much of it do you need, ContentView?
>- (1) ContentView: You can have the whole screen, how much of it do you need, background?
>- (1) Background: You can have the whole screen, how much of it do you need, padding?
>- (1) Padding: You can have the whole screen minus 20 points on each side, how much of it do you need, text?
>- (2) Text: I need X by Y.
>- (2) Padding: I need X by Y plus 20 points on each side.
>- (2) Background: I need X by Y plus 20 points on each side.
>- (2) ContentView: I need X by Y plus 20 points on each side.
>- (3) SwiftUI: OK; I’ll center you.

## Concepts

1. _layout neutral_: a `View` is said to be _layout neutral_ iff it does not have any size of its own, and instead happily adjusts to fit what ever size is needed. (This is the default behaviour of custom views and most `ViewModifier`)
2. _safe area_

## Modifiers 

| modifier | property |
| ---- | ---- |
| `.background()` | layout neutral |
## The Toolbox

### `AlignmentID`

>[!NOTE] Takeaway
> use `AlignmentID` and `.alignmentGuide()` to align elements in different stacks; of cause, better try `Grid` first.

Use a type conforms to the `AlignmentID` protocol to make a customised alignment.

```swift 
protocol AlignmentID { 
  static func defaultValue(in context: ViewDimension) -> CGFloat
}

extension View {
  func alignmentGuide(
      _ g: HorizontalAlignment, 
      computedValue: @escaping (ViewDemensions) -> CGFloat
    ) -> some View

  func alignmentGuide(
      _ g: VerticalAlignment, 
      computedValue: @escaping (ViewDemensions) -> CGFloat
    ) -> some View
}

```

There 2 important functions to control how elements are aligned inside a `HStack/VStack`.
  1. `defaultValue`: position an alignment guide (line) inside the parent container, i.e. `VStack` or `HStack` 
  2. `alignmentGuide`: position an alignment guide (line) on an element child
Then align elements by force all alignment guide lines to the same position

### Grid 

1. Its very hard to add a background to a `GridRow`;
2. use `.gridCellAnchor()` to align a view in its grid cell;
3. use `.gridColumnAlignment()` to align a column differently from the grid's alignment settings;
4. use `.gridCellUnsizedAxes()` to constrain views that takes as much space as possible.

### `GeometryReader`

> In its most basic usage, what `GeometryReader` does is let us read the size that was proposed by the parent, then use that to manipulate our view.

```swift
struct ContentView: View {
    var body: some View {
        GeometryReader { geo in  
            Text("Hello, World!")
                .frame(width: geo.size.width * 0.9)
                .background(.red)
        }
    }
}
```


>[!WARNING]
> `GeometryReader` has an interesting side effect that might catch you out at first: the view that gets returned has a flexible preferred size, which means it will expand to take up more space as needed. 
>
> Usually assumes all remaining space greedily

- `var size : CGSize`: ContentView's container view's size
- `.frame(in: some CoordinateSpaceProtocol) -> CGRect`: ContentView's container view's frame's size and pos measured by some `CorordinateSpace`


### `CoordinateSpace` & `CoordinateSpaceProtocol`

[doc](https://developer.apple.com/documentation/swiftui/coordinatespaceprotocol)

- _The global space_, `CoordinateSpace.global`, measuring our view’s frame relative to the whole screen;
- _The local space_, `CoordinateSpace.local`, measuring our view’s frame relative to its parent.
- _Named space_, a named coordinate space allows you to convert the frame of a view into the local coordinate space of an ancestor view by defining a named coordinate space using the `coordinateSpace(_:)` modifier, then passing that same named coordinate space into the `frame(in:)` function.
    ```swift
    VStack {
      GeometryReader { geometryProxy in
        let distanceFromTop = 
          geometryProxy.frame(in: "container").origin.y
        Text("This view is \(distanceFromTop) points from the top of the VStack")
        }
        .padding()
    }
    .coordinateSpace(.named("container"))   
   ```


>[!Note] Takeaway
>Which coordinate space you want to use depends on what question you want to answer:
> - Want to know where this view is on the screen? Use the global space.
> - Want to know where this view is relative to its parent? Use the local space.
> - What to know where this view is relative to some other view? Use a custom space.


### `.containerRelativeFrame()`

### Advanced
  - `Group`
  - `layoutPriority`


## Custom layout


```swift
protocol Layout { 
  func sizeThatFits(proposal:subviews:cache:) -> CGSize
  func placeSubviews(in bounds:proposal:subviews:cache:)
}
```

- `sizeThatFits`: how large the layout container is;
    - `proposal: ProposedViewSize`: what parent proposes
    - `subViews: Subviews`: proxies of all subviews, to which the layout container can propose size and learn result of proposal
- `placeSubviews`:
    - `in bounds: CGRect`: the region in which all subviews are placed. the size of this CGRect is determined by the above `sizeThatFits` function. _Do not assume the rectangle's origin to be (0, 0)_


## Reference
- [Compose Custom layouts with SwiftUI](https://developer.apple.com/wwdc22/10056)
- [The Complete Guide to Layout in SwiftUI](https://www.hackingwithswift.com/articles/217/complete-guide-to-layout-in-swiftui)
    - [How layout works in SwiftUI](https://www.hackingwithswift.com/books/ios-swiftui/how-layout-works-in-swiftui)
    - [Absolute positioning for SwiftUI views](https://www.hackingwithswift.com/books/ios-swiftui/absolute-positioning-for-swiftui-views)
    - [Using groups as transparent layout containers](https://www.hackingwithswift.com/books/ios-swiftui/using-groups-as-transparent-layout-containers)
    - [Alignment and alignment guides](https://www.hackingwithswift.com/books/ios-swiftui/alignment-and-alignment-guides)
    - [How to create a custom alignment guide (Align views in different stacks)](https://www.hackingwithswift.com/books/ios-swiftui/how-to-create-a-custom-alignment-guide)
    - [Understanding frames and coordinates inside GeometryReader](https://www.hackingwithswift.com/books/ios-swiftui/understanding-frames-and-coordinates-inside-geometryreader)
- [Difference between a ZStack or using .overlay()](https://stackoverflow.com/questions/63446213/difference-between-a-zstack-or-using-overlay) 
- [SwiftTutorial Layering content](https://developer.apple.com/tutorials/swiftui-concepts/layering-content)
- <mark class="hltr-pink">_Inspecting the View Tree_</mark> (how to pass data upward view tree)
    - [Part 1: PreferenceKey](https://swiftui-lab.com/communicating-with-the-view-tree-part-1/)
    - [Part 2: AnchorPreferences](https://swiftui-lab.com/communicating-with-the-view-tree-part-2/)
    - [Part 3: Nested Views](https://swiftui-lab.com/communicating-with-the-view-tree-part-3/)

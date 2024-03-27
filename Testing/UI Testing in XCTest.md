#program-test #ui-test 

XCTest frame work for UI Testing provides a series of _proxy types_:
1. `XCUIApplication` is a proxy for the app under test
2. `XCUIElement` represents some UI element that can be shown, such as button, textField, etc.
3. `XCUIElementTypeQueryProvider` is a protocol that supports you to query `XCUIElement`s satisfying the `XCUIElementQuery` object you provides

the `XCUIApplication` inherits `XCUIElement` and _serves the purpose of a stub or handler_ of a real application, you generally use it to read/write application state.

# Query


## Run a query

```swift
extension XCUIElementQuery { 
  subscript(_ uiElemIdentifier: String) -> XCUIElement 
  /// num of matching elems
  var count : Int 
  /// single matched elem
  var element : XCUIElement
  /// matching element with the provided type and identifier
  func element(
      matching: XCUIElement.ElementType, 
      identifier: String? = nil
    ) -> XCUIElement
  /// An matching element that matches the predicate.
  func element(matching: NSPredicate) -> XCUIElement
}
 
```

>[!note] identifier
> ui identifiers can be attached using the [`accessibilityIdentifier(String)`](https://developer.apple.com/documentation/swiftui/view/accessibilityidentifier(_:)) modifier
## Build a query

you can use `XCElementQueryProvider` to create a `XCElementQuery` and use methods of it self to refine the query:

```swift
extension XCElementQuery { 
  func matching(identifier: String) -> XCUIElementQuery
  func matching(NSPredicate) -> XCUIElementQuery
  func matching(
      XCUIElement.ElementType, 
      identifier: String?
    ) -> XCUIElementQuery

  /// query all elem that contains elems matches the 
  /// specification provided by the argument
  func containing(NSPredicate) -> XCUIElementQuery
  func containing(
      XCUIElement.ElementType, 
      identifier: String?
    ) -> XCUIElementQuery

  /// all direct children of the given type
  func children(matching: XCUIElement.ElementType) -> XCUIElementQuery
  /// all descendants of the given type
  func descendants(matching: XCUIElement.ElementType) -> XCUIElementQuery
}
```

however the most powerful will be making a `NSPredicate` where examples can be found [here](https://nspredicate.xyz)


# What can we do with UI Elements

## inspecting content

most of the UI property is exposed through the `XCUIElementAttributes` [protocol](https://developer.apple.com/documentation/xctest/xcuielementattributes):

```swift
extension XCElement { 
  /// Determines if the element exists.
  var exists: Bool
}

extension XCElement: XCUIElementAttributes {..}

protocol XCUIElementAtrributes {
  var identifier: String
  var value: Any?

  var label: String
  var title: String
  var placeholderValue: String?

  var hasFocus: Bool
  var isSelected: Bool

  var frame: CGRect
  
}
```

## performing user actions

- keyboard typing  
  - `typeText(String)`
  - `typeKey(..)`
  - `perform(withKeyModifiers, block: () -> Void)` Executes a block of code while holding a combination keystroke.
- mouse
  - `hover()`
  - `click()`, `doubleClick()`, `rightClick()`
- scroll: `scroll(x, y)` 
- screen touch
  - `tap`, `doubleTap`, `press`
  - swipe
- etc
# References

[Fundamentals of XCUITest](https://medium.com/tauk-blog/fundamentals-of-xcuitest-7dcbc23c4ee)
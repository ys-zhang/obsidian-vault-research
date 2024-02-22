
# Focusability

```swift 
extension View {
  func focusable(
    _ isFocusable: Bool = true,
    interactions: FocusInteractions
    ) -> some View
}

struct FocusInteractions 
extension FocusInteractions {
  static var automatic : FocusInteractions
  static var active    : FocusInteractions
  static var edit      : FocusInteractions
}
```

# Focus State

Watching focus changes and change focus programatically.

```swift
extension View {
  func focused(_ condition: FocusState<Bool>.Binding) -> some View

  func focused<Value>(
    _ binding: FocusState<Value>.Binding,
    equals value: Value
  ) -> some View where Value : Hashable              
}
```

example 1

```swift 
struct SomeView : View { 
  @FocusState private var focusedItem : Item?
  var body : some View { 
    List(items) { item in 
      ItemView.focused($focused, equals: item)
    }
  }
}

```

# Focus Value

The Focused Values API solves the problem of how to build data dependencies that link remote parts of your user interface to focus state, which is much like the environment value api


# Focus Section 

focus section gives focusing a tree like structure its something like 

```haskell
data Focusable = Elem | FocusSection [Focusable]

-- | the order of all elems getting focus by keep pressing
-- `TAB`
tabOrder :: Focusable -> [Focusable]
tabOrder Elem = [Elem] 
tabOrder (FocusSection xs) =
    concapMap tabOrder $ concat orderedRows
  where
    orderedRows :: [Focusable] -> [[Focusable]]
    orderedRows = fmap sortByColNumber . sortByRowNumber . groupByRows $ xs
    groupByRows :: [Focusable] -> [(RowNum, [Focusable])]
    groupByRows = _
    sortByRowNum :: [(RowNUm, a)] -> [a]
    sortByRowNum = _
```

keep pressing the `TAB` key can take an ***inorder traversal*** of the focus tree.
# References
- [The SwiftUI Cookbook for Focus](https://developer.apple.com/wwdc23/10162)




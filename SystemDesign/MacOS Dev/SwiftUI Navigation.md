# Navigation Stack

[NavigationStack doc][https://developer.apple.com/documentation/swiftui/navigationstack]


```haskell
data NavStack = MkNavStack 
  { viewStack :: [Some View]
  }
  
type Some :: (Type -> Constraint) -> Type
data Some c where
  some :: c a => Some c
```

this _view stack_ is private and managed internally by `NavigationStack`. However, there are 2 method you can manipulate the view with 2 methods:
1. use `NavigationLink` to push views to the _view stack_, the `NavigationLink` is probably implemented using [SwiftUI preferences](https://developer.apple.com/documentation/swiftui/preferences)
2. use _path stack_ binding to modify the _view stack_


```swift
// the path stack binding
@State var pathStack: [SomeModel] = []

var models: [Model]

var body: some View { 
  NavigationStack(path: $pathStack) { 
    // this is the root of the view stack
    List(models) { m in 
      // will be pushed to `pathStack` when clicked
      NavigationLink(m.name, value: m)
    }
    // how to transfer an elem in `pathStack` to a view 
    // in view stack
    .navigationDestination(for: SomeModel.self) { model in 
      ModelDetailView(model: model)
    }
  }
}
```

# NavigationSplitView

  Instead of keeping a history stack like `NavigationStack`, `NavigationSplitView` keeps a list of options from which we choose to show in _detail view_

```swift
@State private var selectedModel: SomeModel.ID? 
// list of option models
var models : [SomeModel] 

var body : some View { 
  NavigationSplitView { 
    // master(sidebar) view
    List(models, selection: $selectedModel) { m in 
        // on selection will 
    }
  } detail: { 
    // detail view
    if let s = selectedModel { 
      SomeDetialView(for: selectedModel)
    }
  }
}
```

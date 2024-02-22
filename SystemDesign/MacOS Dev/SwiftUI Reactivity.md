- `@State`, `@StateObject`

# State, Binding, etc

|  | struct | @Observabler | ObservableObject |
| ---- | ---- | ---- | ---- |
| @State | update when changed | update when dependent field changed | update when object address changed |
| @StateObject | / | / | ref or published variable changed |
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |

- the `ObservableObject` and `ObservedObject` are used in pair, `@ObservedObject` tell SwiftUI to monitor an `ObservableObject`

>[!Takeaway] 
>Use @state with a struct or with a class decorated by `@Observable`

## Concepts

- `State`
    1. Use to create a single source of truth for a given ***value type*** in a view.
    2. Shouldn't be used for ***reference types*** as SwiftUI won't update the view when one of its properties changes.
- `Binding`
    1. Has a `getter` and a `setter`; 
    2. Use to create a two-way connection between a property that stores data and a view that displays and changes the data;
    3. It connects a property to a source of truth stored elsewhere instead of storing data directly.
- `Bindable`
    1. Use to create bindings to mutable properties of a data model object that conforms to the `Observable` protocol.
    2. Can also be used in conjunction with `Environment`
        ```swift
        @Environment(Profile.self) private var profile: Profile 
        var body : some View {
          // must be placed in view body 
          @Bindable var profile = profile 
          // Binding object can be extracted with the help of Bindable
          TextField("Name", text: $profile.name) 
        }
        ```

## After iOS17
|  | State view | Sub view |
| ---- | ---- | ---- |
| Value Type | `@State` | `@Binding` |
| Ref Type | `@State` | `@Binding`<br> |
| Ref Type | plain object | `@Bindable` |

N.B. 
- the `@Bindable` property wrapper creates a binding of the type at the first time when some object is assigned to it

## Pre iOS17
|  | State view | Sub view |
| ---- | ---- | ---- |
| Value Type | `@State` | `@Binding` |
| Ref Type<br> | `@StateObject` | `@ObservedObject` |


## References

1. [Why @Bindable and not @Binding in SwiftUI for iOS 17](https://forums.developer.apple.com/forums/thread/735416#:~:text=So%2C%20to%20answer%20your%20original,the%20properties%20of%20observable%20objects.)
2. [Manage model data in your app](https://developer.apple.com/documentation/swiftui/managing-model-data-in-your-app)
3. [Migrating from the Observable Object protocol to Observable macro](https://developer.apple.com/documentation/swiftui/migrating-from-the-observable-object-protocol-to-the-observable-macro)
4. [WWDC23-Discover Observation in SwiftUI](https://developer.apple.com/wwdc23/10149)


# Combine

```swift
protocol Publisher<Output, Failure> {
  // attaches the subscriber to this publisher
  func receive<S>(subscriber: S)
}
```

## Misc


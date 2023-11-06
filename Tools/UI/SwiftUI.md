
# Components

## Protocols 

1. `View`
2. `Codable`, this is just `Serializable + Deserializable` in _rust_
## Layout

1.`Capsule` A capsule shape aligned inside the frame of the view containing it. A _capsule shape_ is equivalent to a _rounded rectangle_ where the corner radius is chosen as half the length of the rectangle’s smallest edge.

## Views

1. `List`, 
2. `ForEach`, same as _Leptos_'s [`For`](https://docs.rs/leptos/latest/leptos/fn.For.html)
3. `NavigationView`
4. `ScrollView`
5. `ZStack`, `overlay(alignment:content:)` and `background(_:in:fillStyle:)`
  - `overlay` puts views on top of the caller object, 
  -  `background` puts views bellow the caller object 
6. `if else`, `opacity` and `hidden`
  - `if else` re-render every time
  - `opacity` renders and take up the space, only invisible
  - `hidden` no render except taking up the space
7. `Spacer` occupies space as much as possible
8. `NavigationView`, `NavigationStack`, `NavigatonLink`
9. `Group`, `Section`


## Modifiers 
`.swipeActions`
`.sheet` 
# Misc 

- `State` is a [[#Property Wrapper]] applying to some _value type_
- `StateObject` is `State`'s counterpart version for _reference type_
- `Binding` is another _property wrapper_ 

>[!Note]
>The `State` is just what a `Signal` in _Leptos_, and a `Binding` is 
>more alike a readonly signal, i.e., function in _Leptos_

# Swift Concepts

## Property Wrapper

- `s.field` _getter_ and _setter_ uses the `wrappedValue` property
- `s.$field` uses the `projectedValue` property
- `s._field` is the outer wrapping instance.

```swift 
/// the `propertyWrapper` attribute creates a
/// an attribute `SomeWrapper` that can annotate a 
/// property at declaration
@propertyWrapper
struct SomeWrapper { 
    /* the value that the getter and setter for 
       this property expose */ 
    var wrappedValue: Int 
    /// `projectedValue` can be accessed by `$` prefix
    /// @SomeWrapper var p ...
    /// a.p  // wrapped value
    /// a.$p // projected value 
    var projectedValue : SomeCustomType { return SomeCustomType(...) }
    /// some value not accessible outside
    var someValue: Double
    
    /// case: @SomeWrapper var a : Int
    init() {
        self.wrappedValue = 100
        self.someValue = 12.3
    }
    /// case: @SomeWrapper var a: Int = 1
    init(wrappedValue: Int) {
        self.wrappedValue = wrappedValue
        self.someValue = 45.6
    }
    /// case: @SomeWrapper(custom: 98.7) var a: Int = 1
    init(wrappedValue value: Int, custom: Double) {
        self.wrappedValue = value
        self.someValue = custom
    }
}
```

## Key Path Expression

Syntax:
> \<#type name#>.<#path#>

the reference type `KeyPath<Root, Value>` represents a function
```swift
(Root) -> Value

let value = s[keyPath: \SomeType.someValue]
```


# Frameworks

- `Combine` framework customise handling of _asynchronous events_ by combining event-processing operators.
  - defines `Observable` and `ObservableObject`, together with `EnvironmentObject` gives the dependency injection functionality and _redux_ like global store


# Topics

## Animation

There are 2 places where you can add animation:
1. add animation to a `View`:
>   When you use the `animation(_:)` modifier on an _equatable_ view, SwiftUI animates any changes to _animatable properties_ of the view. 
>   
>   _A view’s colour, opacity, rotation, size, and other properties are all animatable._ 
>   
>   When the view isn’t _equatable_, you can use the `animation(_:value:)`modifier to start animations when the specified value changes.
2. add animation to _updates of data_, i.e., smoothing/interpolating state changes:
    - `withAnimation`
3. `transition` to add effects to `Views`


# Reactivity

## Combine



# Language 

## Struct and copy

copy happens at _move_, i.e., assign & argument passing

```swift 

struct S { 
  var c : C = C()
}

class C { 
  var v : Int = 0
}

var s1 = S() 
// s1 copied, but copy of a reference is the same referece
// i.e., s1.c is s2.c, pointing to the same memory
var s2 = s1    
print(s2.c.v)  // 0 
s1.c.v = 1
print(s2.c.v)  // 1
```

# FAQ 

1. `A server with the specified hostname could not be found.`, enable `Outgoing Connections` to `Target > Capabilities > App Sandbox`

# References 

- [Result Builders](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/advancedoperators/#Result-Builders)
- [Property Wrapper](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/attributes/#propertyWrapper)
- [Key Path Expression](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/expressions/#Key-Path-Expression)

# Target Action Design Pattern

[Tut](https://developer.apple.com/documentation/uikit/views_and_controls/responding_to_control-based_events_using_target-action)

> When a person interacts with your _control_, the _control_ needs to know where to send the event. The destination for the event is the ***target***. View controllers make good targets because they’re adept at handling user interactions, as well as hosting UI controls.

![[Pasted image 20240108110608.png]]

```swift
@IBAction func doSomething()
@IBAction func doSomething(sender: Any)
```
- `@IBAction` does two things:
    1. connect controls in _Interface Builder_ to functions in the code file
    2. bridges Swift runtime and ObjC runtime, since the _target-action_ functionality is implemented in ObjC
- the `sender` argument lets you inspect where the event come from. Use the generic type `Any` to allow any control to call the action method, or specify the `sender` type when you need more information about the control for event processing.

# Concepts

## Delegate

A _delegate_ is any object that should be notified when something interesting has happened.

What that "something interesting" means depends on the context: for example, a table view's delegate gets notified when the user taps on a row, whereas a navigation controller's delegate gets notified when the user moves between view controllers.

```swift
class SomeOberserver<T: Notifiable> { 
  var delegate :  T
  // call delegate.onNotification when some thing 
  // interesting happends
}

protocol Notifiable { 
  func onNotification(msg: String) { 
  }
}
```

>[!Example]
>An `NSApplication` object has a delegate (an object that you assign) that’s notified when the app starts or terminates, is hidden or activated, should open a file selected by the user, and so forth. By setting the delegate and implementing the delegate methods, you customize the behavior of your app without having to subclass `NSApplication`.


# SwiftUI AppKit interop


## Hosting Appkit `NSView` in SwiftUI `View`

The idea is to use an object conforms to `NSViewRepresentable` to control how to create, update and deconstruct the `NSView` it represents(or hosts). In other words, this `NSViewRepresentable` is a declarative statement about how to CRUD `NSView`

### Hosting lifecycle

1. initialise hosted `NSView`, happened when the view is about to be displayed in the first time, in which
  1. make a coordinator using `makeCoordinator`, _this single instance of Coordinator will stay around during the whole life time of the view_
  2. call `makeNSView`/`makeNSViewController` with the `context` to be the `Coordinator` created in the 1st step; often, here is the place to _assign the context to the `NSView`'s delegator_
2. update the view, `@State` or `@Environment` will trigger the `updateNSView` function  
3. `dismantalNSView`



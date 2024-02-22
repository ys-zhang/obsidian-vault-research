 
# Takeaways

1. Do not instantiate (or referencing) system fonts by name
  - Never use font names with a dot prefix
2. Font instantiation by name is not guaranteed
  - Fonts maybe replaced
  - User installed font may go away

>[!NOTE] Font Provider App
> Apps now can install fonts in the OS
> these fonts to be installed can be
> - part of a _bundle_
> - inside some _asset catalog_
3. `relativeTo`, _dynamic types_(e.g. `.title`, `.body`) contains a scaling factor, when called `someFontFunc(..., size: s, relativeTo: .title)`, the real size will be `s * scalingFactor`.
4. Apple has **11 text styles** at the moment: Large Title, Title 1, Title 2, Title 3, Headline, Body, Callout, Subhead, Footnote, Caption 1, and Caption 2. These font styles uses _Dynamic Type_.
 5. there are other 2 types of fonts: `custom` and `system`,
   - `custom` looks up the _Font provided by application_ info.plist key; 
   - `system` looks up font installed in the os's system font book.
# Best practice

## Requirements

1. App should be able to respond to font change notifications
2. use [_on-demand resources_](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/On_Demand_Resources_Guide/) for large font libraries


## Prerequisite: Capabilities

There is a capability called _Fonts_ that will allow the app to _install_ and _use_ user-installed fonts.


## Api for fonts

The apis are functions defined in Framework `Core Text` under topic _Core Text Functions_ and start with _CTFontManager_

1. Register/Unregister Fonts: _CTFontManagerRegisterXXXXXX_
    - `CTFontManagerRegisterFontUTLs`
    - `CTFontManagerRegisterFontDescriptors`
2. Query installed Fonts:
    - `CTFontManagerRequestFonts`
3. User `NotificationCenter` to listen for `kCTFontManagerRegisteredFontsChangedNotification`

Swift Types:
- [`NSFont`](https://developer.apple.com/documentation/appkit/nsfontdescriptor)

# References

1. https://developer.apple.com/documentation/swiftui/applying-custom-fonts-to-text
2. https://developer.apple.com/documentation/uikit/uifont/scaling_fonts_automatically
3. [(WWDC19) Font Management and Text Scaling](https://developer.apple.com/wwdc19/227)
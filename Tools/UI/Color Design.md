


# Material Design
- [The colour system - Material Design](https://material.io/design/color/the-color-system.html)

## Theme

![[Pasted image 20220321192016.png]]

- A **primary color** is the color displayed most frequently across your app's screens and components.
- A **secondary color** provides more ways to accent and distinguish your product.
    - _Floating action buttons_
    - _Selection controls_, like sliders and switches
    - Highlighting _selected text_
    - _Progress bars_
    - _Links and headlines_
- **Surface colours** affect surfaces of components, such as _cards, sheets, and menus_.
- **Background colour** appears behind scrollable content
- **Error color** indicates errors in components, such as invalid text in a text field.
- **“on” colours**, referring to the fact that they color _elements that appear “on” top of surfaces_ that use the following colours: a primary color, secondary color, surface color, background color, or error color.
    - “On” colours are primarily applied to text, iconography, and strokes. 
    - Sometimes, they are applied to surfaces.
- **Colour variants**: to ensure an _accessible background_ behind light or dark text, your background can use light or dark variants of your primary and secondary colours.


#### Color Swatch

![[Pasted image 20220321193012.png]]

A **swatch** is a sample of a color chosen from _a range of similar colours_.

• A **white check mark** indicates when white text is legible on a background color  
• A **black check mark** indicates when black text is legible on a background color

#### Alternative Color & Section Theme

**Alternative colours** can be used to theme different parts of an app.
![[Pasted image 20220321193331.png]]

## Text Color

Contrast by displaying white or black text with **reduced opacity**.

![[Pasted image 20220321212113.png]]

![[Pasted image 20220321212342.png]]


![[Pasted image 20220321212219.png]]



# Fluent Design

- [Windows 应用中的颜色 - Windows apps | Microsoft Docs](https://docs.microsoft.com/zh-cn/windows/apps/design/style/color)


Theming in windows contains:
1. _Dark_, *light* and *high contrast* theme that affects the colours of the app's _background, text, icons, and [common controls](https://docs.microsoft.com/en-us/windows/apps/design/controls/)_. 
2. An accent colour which is usually selected by the user.

Unlike in [[#Material Design]], windows use [theme brushes](https://docs.microsoft.com/en-us/windows/apps/design/style/xaml-theme-resources#the-xaml-color-ramp-and-theme-dependent-brushes) to model the semantics of theme colours.

#### Accent Color

![[Pasted image 20220321203010.png]]

These shades can be accessed as [theme resources](https://docs.microsoft.com/en-us/windows/apps/design/style/xaml-theme-resources):

-   `SystemAccentColorLight3`
-   `SystemAccentColorLight2`
-   `SystemAccentColorLight1`
-   `SystemAccentColorDark1`
-   `SystemAccentColorDark2`
-   `SystemAccentColorDark3`

![[Pasted image 20220321203145.png]]

![[Pasted image 20220321203225.png]]
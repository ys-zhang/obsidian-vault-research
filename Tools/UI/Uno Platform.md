# Template Structure

Normally:

  1.  UI and business logic will go in the shared project.
  2.  Bootstrapping code, packaging settings, and platform-specific code goes in the corresponding platform head.
  3.  String resources normally go in the shared project. 
  4.  Image assets may go either in the shared project or under each head. 
  5.  Font assets must be placed under each head.
    

## Shared Project

A _shared project_ is just a list of files

Referencing a _shared project_ in an ordinary `.csproj` project causes those files to be included in the project.

They're treated in exactly the same way as the files inside the project.

- Code in a shared-project file is compiled separately for each platform head.
- A shared project is just a list of files, it can't depend on other projects, NuGet packages, or system assemblies.
- Dependencies must be added to the projects that are referencing the shared project.
- File under shared project can be considered to be copied under each platform specific head.    

## Platform-specific Code

Use [conditionals](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/preprocessor-directives/preprocessor-if) within a _shared file_:

| Platform    | `#if` conditional |
| ----------- | -------------- |
| UWP         | `NETFX_CORE`   |
| Android     | `__ANDROID__`  |
| iOS         | `__IOS__`      |
| WebAssembly | `HAS_UNO_WASM` |
| macOS       | `__MACOS__`    |
| Skia        | `HAS_UNO_SKIA` |

Use type alias
```c#
#if __ANDROID__
using _View = Android.Views.View;
#elif __IOS__
using _View = UIKit.UIView;
#else
using _View = Windows.UI.Xaml.UIElement;
#endif
```

Use Partial Class

For XAML:

- `mc:Ignorable`
- condition markups

```xml
<Page x:Class="HelloWorld.MainPage"
	  xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
	  xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
	  xmlns:android="http://uno.ui/android"
	  xmlns:ios="http://uno.ui/ios"
	  xmlns:wasm="http://uno.ui/wasm"
	  xmlns:win="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
	  xmlns:not_android="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
	  xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
	  xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	  mc:Ignorable="d android ios wasm">

	<StackPanel Margin="20,70,0,0">
		<TextBlock Text="This text will be large on Windows, and pink on WASM"
				   win:FontSize="24"
				   wasm:Foreground="DeepPink"
				   TextWrapping="Wrap"/>
		<TextBlock android:Text="This version will be used on Android"
				   not_android:Text="This version will be used on every other platform" />
		<ios:TextBlock Text="This TextBlock will only be created on iOS" />
	</StackPanel>
</Page>
```

# OneDrive Synchronization

- `Bogus.NET` is a robust library useful providing fake data in different categories (finance, country, personal, health care etc.) 
- The simplest way to make access OneDrive in an Uno Platform app is to use the `Microsoft Graph.NET SDK` and the `Microsoft Identity .NET` package.
    

# Adaptive Styling, Layouts & Controls

In the Microsoft XAML space, `ThemeResource` provides a means for obtaining values for XAML attributes that are defined in XAML resource dictionary at runtime.

In Uno platform, we can use the XAML markup `ResourceDictionary.ThemeDictionaries` to create special dictionaries with a similar key but different values. (Different themes with same key defined with different values)
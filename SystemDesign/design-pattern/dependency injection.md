在软件工程中，依赖注入是一种为一类对象提供依赖的对象的设计模式。被依赖的对象称为`Service`，注入则是指将被依赖的对象`Service`传递给使用服务的对象(称为`Client`)，从而客户`Client`不需要主动去建立(new)依赖的服务`Service`，也不需要通过工厂模式去获取依赖的服务`Service`。

[Dependency injection in .NET | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/core/extensions/dependency-injection)
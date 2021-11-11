COM is used to enable inter-process communication object creation in a large range of programming languages.

COM is the basis for several other Microsoft technologies and frameworks, including OLE, OLE Automation, Browser Helper Object, ActiveX, COM+, DCOM, the Windows shell, DirectX, UMDF and Windows Runtime. 

> The essence of COM is a language-neutral way of implementing objects that can be used in environments different from the one in which they were created, even across machine boundaries.

[组件对象模型 (COM) - Win32 apps | Microsoft Docs](https://docs.microsoft.com/zh-cn/windows/win32/com/component-object-model--com--portal)

 it is not an object-oriented language but a standard. COM specifies an object model and programming requirements that enable COM objects (also called COM components, or sometimes simply _objects_) to interact with other objects.
 
 These objects can be within a single process, in other processes, and can even be on remote computers. They can be written in different languages, and they may be structurally quite dissimilar, which is why COM is referred to as a _binary standard_; a standard that applies after a program has been translated to binary machine code.
 
 
 #### specification
 
 The COM specification provides all of the fundamental concepts that enable _cross-platform software reuse_:

-   A binary standard for function calls between components.
-   A provision for strongly-typed groupings of functions into interfaces.
-   A base interface that provides polymorphism, feature discovery, and object lifetime tracking.
-   A mechanism that uniquely identifies components and their interfaces.
-   A component loader that creates component instances from a deployment.


#### structure 
COM has a number of parts that work together to enable the creation of applications that are built from reusable components:

-   A _host system_ that provides a run-time environment that conforms to the COM specification.
-   _Interfaces_ that define feature contracts, and _components_ that implement interfaces.
-   _Servers_ that provide components to the system, and _clients_ that use the features provided by components.
-   A _registry_ that tracks where components are deployed on local and remote hosts.
-   A _Service Control Manager_ that locates components on local and remote hosts and connects servers to clients.
-   A _structured storage_ protocol that defines how to navigate the contents of files on the host's file system.


Enabling code re-use across hosts and platforms is central to COM. ==A reusable interface implementation is named a _component_, a _component object_, or a _COM object_.== A component implements one or more COM interfaces.
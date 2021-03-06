# CLR (Common Language Runtime)

## Dynamic loading in DotNet

[Dependency loading - .NET | Microsoft Docs](https://docs.microsoft.com/en-au/dotnet/core/dependency-loading/overview)

# Data


## Database

There are several technologies that can provide database access/manipulation for dotnet:

| Standards                           | DB                  | Misc.                | Conn String                                                                               |
| ----------------------------------- | ------------------- | -------------------- | ----------------------------------------------------------------------------------------- |
| _ADO.NET_                           |                     |                      |                                                                                           |
| _ADO_ (ActiveX Data Objects)        | RDBMS, NoSQL, Excel | COM wrapper of OLEDB | same as OLEDB                                                                             |
| _OLEDB_                             | RDBMS, NoSQL, Excel |                      | `"provider = sqloledb; Data Source = servername; uid = xx; Pwd = xxx; database = dbname"` | 
| _ODBC_ (Open Database Connectivity) | RDBMS               |                      |                                                                                           |


###  Namespace `System.Data` & ADO.NET

![[Pasted image 20220203222003.png]]

```csharp
OracleConnection connection = new OracleConnection(
      "data source=.; database=TestDB; integrated security=SSPI"
  );
OracleCommand command = new OracleCommand("Select * from Customers", 
                                          connection);
connection.Open();
OracleDataReader myReader = command.ExecuteReader();

while (myReader.Read())
{
     Console.WriteLine("\t{0}\t{1}", myReader.GetInt32(0), 
                       myReader.GetString(1));
}

connection.Close();
```

- Provider specific: `Connection`, `Command`, `DataReader`, `DataAdapter`
- Provider independent: `DataSet`


## ADO.NET

From the remarks of the reference of `System.Data`:

| Concept                | Description                                                                |
| ---------------------- | -------------------------------------------------------------------------- |
| `DataSet`              | contains multiple `DataTable`                                              |
| `DataTable`            | represents data from a single data source, contains `DataColumnCollection` |
| `DataColumnCollection` | defines the _schema_ of the table, contains multiple `DataColumn`          |
| `DataColumn`           | determines the type of data it holds                                       |


## Data Analysis Tools

### Deedle
#todo 



## References
- [Introduction to ADO.NET Framework - Dot Net Tutorials](https://dotnettutorials.net/lesson/what-is-ado-net/)
- [A Brief history of ODBC, OLEDB, ADO, and ADO. Net Evolution (alibabacloud.com)](https://topic.alibabacloud.com/a/brief-history-of-odbc-oledb-ado-and-ado-net-evolution_8_8_32314220.html)
- [`System.Data` Namespace provides access to classes that represent the _ADO.NET_ architecture](https://docs.microsoft.com/en-us/dotnet/api/system.data?view=net-6.0)
- Excel :
  - [ExcelADO demonstrates how to use ADO to read and write data in Excel workbooks (microsoft.com)](https://support.microsoft.com/en-us/topic/excelado-demonstrates-how-to-use-ado-to-read-and-write-data-in-excel-workbooks-bfb26f12-ba6a-91be-7fd4-4aadf1ff1afa)
  - [Read and Import Excel Sheet using ADO.Net and C# (aspsnippets.com)](https://www.aspsnippets.com/Articles/Read-and-Import-Excel-Sheet-using-ADO.Net-and-C.aspx)
  - [Access Database Engine - Wikipedia](https://en.wikipedia.org/wiki/Access_Database_Engine), the Access Database Engine is formerly called Microsoft Jet Database engine.
  - [NPOI](https://baike.baidu.com/item/NPOI/10374941)
- [ADO.NET | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/)



# Reactive 

## Data binding

[Data binding - UWP applications | Microsoft Docs](https://docs.microsoft.com/en-us/windows/uwp/data-binding/)


Three elements in data binding

1. bind source
2. bind target
3. bind object: `Bind` and `x:Bind`

There are three modes of binding:

| Mode     | Meaning       | Supporting     |
| -------- | ------------- | -------------- |
| One time | fixed value   | Class Property |
| One way  | read only     | observable     |
| Two way  | read & update | observable     | 

To be _observable_ the `class` of the binding source should be any of:

1. derive from `DependencyObject`, and expose a data value through a `DependencyProperty`.
2. implement `System.ComponentModel.INotifyPropertyChanged`
3. drive from `BindableBase`


### Bind to Function


## Delegate

```c#

// Define the delegate type:
//   Just like declaration of a function
public delegate int Comparison<in T>(T left, T right);
// Declare an instance of that type:
public Comparison<T> comparator;
int result = comparator(left, right);  // Invoke a delegate
```


>[!NOTE]
> The most important fact to remember is that every delegate you work with is derived from `MulticastDelegate`. 
> A _multicast delegate_ means that _more than one_ method target can be invoked when invoking through a delegate.
> 
> - `Invoke()`: invoke all the methods that have been attached to a particular delegate instance
> - `BeginInvoke()`:
> - `EndInvoke()`



## Event

- [Introduction to events | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/events-overview)
- [Events - C# Programming Guide | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/events/)
- [Events - F# | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/members/events)


```c#
// declare an event source
public event EventHandler<T> Event;
EventHandler<FileListArgs> action = (sender, t) => {return;} // T -> unit
// raise the event
Event?.Invoke(this, new T());
Event += action;  // subscribe
Event -= action;  // unsubscribe
```

- The type of the _event_ source (`EventHandler<FileListArgs>`) must be a _delegate_ type.
- `.?` make sure raising nothing when the event has not been
- subscribe to an event by using the `+=` operator
- unsubscribe using the `-=` operator

# WinUI3

## ListView

[Uwp ListView Tips And Common Mistakes ?? Yet Another Tech & Life blog (arashadbm.com)](http://www.arashadbm.com/post/uwp-listview-tips/)

> [!NOTE]  ListView Cannot Scroll
> 
> Put `ListView` in a `Grid` and set the `RowDefination` Height to `"*"`


# Interop of C# & F#

[Connel Hooley - Calling F# Code in a C# Project](https://connelhooley.uk/blog/2017/04/30/f-sharp-to-c-sharp)

- F# projects do not have a _default namespace_ option in their project settings. If you do not put a module in a namespace, then it becomes available globally in any C# projects that reference its project.
- F# _modules_ are exposed to C# as _static classes_. (A static class is basically the same as a non-static class, but there is one difference: a static class cannot be instantiated.)
- In F#, _functions_ are curried and can be partially applied. When accessing an F# function from C#, this functionality is not available. You must provide all parameters to a function when calling it.
    - The F# function's _single tuple parameter_ gets exposed as _multiple parameters_ to C#.
- F# _records_ are exposed to C# as _classes_ that take in all of their properties in their constructor.
    - All properties are `readonly` (getters only)
    - Properties are populated via the constructor
    -  Various comparison interfaces are implemented for you
- F# `sequence` is exposed as C# `IEnumerable`
- Discriminated Union
    - Discriminated union that contain values are compiled down to `enum`
    - Discriminated unions however, have cases that are either empty or contain types.
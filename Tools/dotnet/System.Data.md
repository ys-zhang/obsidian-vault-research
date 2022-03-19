
# `System.Data`

## Data Set

A `DataSet` provides the functionality of managing
  - `DataTable`
  - `DataRelation` 

```c#
using System.Data;

DataSet dataSet = new DataSet("Workspace");
DataTable tbl =  dataSet
                .Tables   // collection of tbls
                .Add("New Table");
var theSameTable = dataSet.Tables["New Table"];
```


### Copy Data Set

- Only copy the schema: `dataSet.Clone()`
- Copy all: `dataSet.Copy()`

## Data Table

```ad-note
A **DataTable** represents one table of in-memory relational data; the data is local to the .NET-based application in which it resides, but can be populated from a data source such as Microsoft SQL Server using a **DataAdapter**
```

- `dataTable.Columns`, `dataTable.Constrains`: defines the schema of the table
- `dataTable.Rows`: holding the actual data.

### Create

- constructor
- `dataAdapter.Fill`, `dataAdapter.FillSchema`

### Schema and Column

- Expression Columns:
    - use the [Expression](https://docs.microsoft.com/en-us/dotnet/api/system.data.datacolumn.expression) property of the target column
    - use the [ColumnName](https://docs.microsoft.com/en-us/dotnet/api/system.data.datacolumn.columnname) property to refer to other columns in the expression.
- Auto Increment Columns
    - `col.ReadOnly=true`
    - `col.AutoIncrement=true`
    - `col.AutoIncrementSeed=110`: start with $110$;
    - `col.AutoIncrementStep=13`: in step of $13$.



### MVCC (multi-version control system)

| Row Version |
| ----------- |
| Current     |
| Original    |
| Proposed    |
| Default     |

| Row State |
| --------- |
| Unchanged |
| Added     |
| Modified  |
| Deleted   |
| Detached          |

When `AcceptChanges` is called on a [DataSet](https://docs.microsoft.com/en-us/dotnet/api/system.data.dataset), [DataTable](https://docs.microsoft.com/en-us/dotnet/api/system.data.datatable) , or [DataRow](https://docs.microsoft.com/en-us/dotnet/api/system.data.datarow):
1. Rows with a row state of `Deleted` are removed.
2. Remaining rows are given a row state of `Unchanged`.
3. The values in the `Original` row version are overwritten with the `Current` row version

## Data Relation

You can use [DataRelation](https://docs.microsoft.com/en-us/dotnet/api/system.data.datarelation) objects to
1. relate one table to another
2. navigate through the tables
3. return child or parent rows from a related table.

Contents of a `DataRelation`:
- name
- array of `DataColumn` refs specifies parent and child column relation

### Foreign Key
```c#
/*
  DataRelation:
    parent: ParentTable
      - ParentColumn: UniqueConstraint
    child: ChildTable
      - ChildColumn: ForeignKeyConstraint
*/
dataSet
  .Relations
  .Add("ForeighKeyRel", // rel name
        // parent column
        dataSet.Tables['parTbl'].Columns['parCol'],
        // child column
        dataSet.Tables['cldTbl'].Columns['cldCol']);

// lookup through foreign key
var rel = dataSet.Relations['ForeignKeyRel'];
foreach(DataRow prow in dataSet.Tables['parTbl'])
{
  foreach(DataRow crow in prow.GetChildRows(rel))
  {
    ...
  }
}
```

### Primary Key / Multi-Index
A primary key is a list of columns.

```c#
workTable.PrimaryKey = 
    new DataColumn[] { workTable.Columns["CustLName"],
                       workTable.Columns["CustFName"] };
```


### Constrains

```c#
ForeignKeyConstraint custOrderFK =  
  new ForeignKeyConstraint(
    "CustOrderFK", 
    custDS.Tables["CustTable"].Columns["CustomerID"], 
    custDS.Tables["OrdersTable"].Columns["CustomerID"]
  ); 
// Cannot delete a customer value
//   that has associated existing orders.
custOrderFK.DeleteRule = Rule.None; 
custDS.Tables["OrdersTable"].Constraints.Add(custOrderFK);
```


## Data Manipulation

### Add rows

```c#
DataRow workRow = workTable.NewRow();
workRow[0] = 1;
workRow["CustLName"] = "Smith";  
workTable.Rows.Add(workRow);
// or
workTable.Rows.Add(new Object[] {1, "Smith"});
```

### Viewing

```f#
val DataTable.Select: ... -> DataRow array

val DataTable.Select: string * string * DataViewRowState -> DataRow array

workTable.Select(
  "CustID > 5",
  "CustLName ASC",
  DataViewRowState.CurrentRows
)
```

### Load data

```c#
// System.Data.Common.DbDataReader impl IDataReader
public void Load (System.Data.IDataReader reader);
```

### Merge: Union and Cross Join

Cross join requires a [[#Primary Key]] defined in the table schema, and `MissingSchemaAction=MissingSchemaAction.Add`.

The semantics of merging:
1. _rows with same primary key_
    - _columns already exists_: original row data is modified;
    - _new columns_: controlled by `MissingSchemaAction` parameter
2. _rows with same primary key_: appended

```ad-note
title: table namespace
Starting with version 2.0, merging causes two new `DataTables` to be created in the target `DataSet`. The original `DataTables` will be unaffected by the merge.
``````


```ad-note
title: PreserveChanges

[PreserveChanges](https://docs.microsoft.com/en-us/dotnet/api/system.data.loadoption#system-data-loadoption-preservechanges), which specifies whether or not to preserve the changes in the existing `DataSet`.


- `PreserveChanges=true`: Incoming values do not overwrite existing values in the `Current` row version of the existing row.
- `PreserveChanges=true`: Overwrite `Current` row version of existing `DataSet`.

```


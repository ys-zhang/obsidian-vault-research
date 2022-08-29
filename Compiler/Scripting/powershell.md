# Commands


###### Start-Job

```powershell
Start-Job
```


## Provider operation

  

### Item

- `New-Item`, `Set-Item`, `Get-Item`

- `Get-ChildItem`: alias `ls`, `dir`

- `Copy-Item`, `Move-Item`, `Rename-Item`

- `Invoke-Item` performs the default action on one or more items

- `Clear-Item` Deletes the contents of one or more items, but does not delete the items

- `Remove-Item`

  

- `Get-Content`: alias `cat`

- `Add-Content`, `Set-Content`, `Clear-Content`

  

### Path

- `Convert-Path`: Converts a path from a PowerShell path to a PowerShell provider path

- `Join-Path`: Combines a path and a child path into a single path

- `Resolve-Path`: Resolves the wildcard characters in a path

- `Split-Path`: Returns the specified part of a path

- `Test-Path`: Determines whether the elements of a path exist or if a path is well formed

  
  

### Current Location

The _current working location_ is the default location to which commands point.

A PowerShell host may have multiple _drives_, in which case, each drive has its own _current location_.

  

The current working location can be saved on a stack, and then set to a new location.

Later, that saved location can be restored from that stack and made the current working location.

  

 There are two kinds of location stacks:

 1. _default working location stack_ or _current working location stack_

 2. _named working location stacks_

  

- `Set-Location`

- `Get-Location`

- `Push-Location`

- `Pop-Location`

  

## Process

- `Get-Process`

- `Get-Service`

 - `Get-Service -Name w32time | Select-Object -Property Status, Name, DisplayName, ServiceType`

 - `dir | Select-Object Name, Length`

- `Stop-Service`

- `Stop-Process`: alias `kill`

  

## Get help

- `Get-Help -Name 'Write-Output' -Full`

- `Get-Command`: search for commands

 - `Get-Command Write-*`

 - `Get-Command -ParameterType SomeClass`: commands have a parameter that accepts SomeClass

 - `Get-Command -Module ActiveDirectory`: commands under module

- `Get-Member` helps you discover what objects, properties, and methods are available for commands.

  

## Functional

- `Where-Object`: alias `where`, `?`;

 - `Get-Service | Where-Object Name -eq w32time`

 - `Get-Service | Where-Object CanPauseAndContinue | Select-Object -Property DisplayName, Status`

- `ForEach-Object`: alias `foreach`

- `Group-Object`: alias `group`

- `$_` or `$PSItem`: the current object, like `it` in kotlin

  
  

```powershell

'ActiveDirectory', 'SQLServer' |

 ForEach-Object {Get-Command -Module $_} |

 Group-Object -Property ModuleName -NoElement |

 Sort-Object -Property Count -Descending

  

Import-CSV $Path | Group-Object -AsHashtable -Property email

  

Get-ChildItem | ForEach-Object {Write-Output "$($_.BaseName).woo"}

  

```

## Convert to / from

- `ConvertTo-Json` convert hashmap to json

 - `$person | ConvertTo-Json`

- `ConvertTo-Csv`

- `ConvertTo-Html`

- `ConvertTo-Xml`

  

- `ConvertFrom-Json` convert hashmap to json

 - `cat ./person.json | ConvertFrom-Json`

- `ConvertFrom-Csv`

- `ConvertFrom-Html`

- `ConvertFrom-Xml`

  

### Export/Import

- `Export-Csv`

- `Import-Csv`

  

## Formats

  

- `Format-Table`: alias `ft`

- `Format-List`: alias `fl`

- `Format-Wide`: alias `fw`

  

## Alias

  

- `Get-Alias`

 - `Get-Alias -Definition Get-Command, Get-Member`

  

## Provider

  

- `Get-PSProvider`

- `Get-PSDrive`

  

## CIM (Common Information Model)

replaces WMI (Windows Management Instrumentation)

  

## Misc

  

- `Write-Output`

- `Get-Date -format "yyyy-MM-dd"`

- `Compress-Archive -Path './app' -CompressionLevel 'Fastest' -DestinationPath "./backup-$date"`

  

- `Get-Verb` all legal verb can be used to name a function

  
  
  

#### PowerShellGet module

- `Find-Module` apt search

- `Install-Module`

 - `Find-Module -Name MrToolkit | Install-Module`

  

# Execution policy

  

`Get-ExecutionPolicy` & `Set-ExecutionPolicy`

  

Policy includes:

- `Unrestricted`

- `Restricted`: can't execute script, can only execute cmdlet

- `RemoteSigned` 只可以运行在本地计算机上编写的脚本， 无法运行internet下载的

- `Default`

  
  

# Run

  

- source the file: `. ./script.ps1`

- just run: `./script.ps1`

  
  

# Language

  

## Variable

  

```powershell

$PI = 3.14

Write-Host 'Here is $PI' # Prints Here is $PI

Write-Host "Here is `$PI and its value is $PI" # Prints Here is $PI and its value is 3.14

Write-Host "An expression $($PI + 1)" # Prints An expression 4.14

```

  
  

```powershell

# Number

[int]$i = 10 # declare int variable i

$i -le 10 # $True

$i -gt 10 # $False

  

# List

[int[]]$Numbers = 1..10 # 1,2,..,10

$Numbers -contains 15 # $False

$Numbers -notcontains 15 # $True

15 -in $Numbers # $False

10 -notin $Numbers # $False

  

# String

'PowerShell' -replace 'Shell' # Power

'PowerShell' -eq 'powershell' # $True

'PowerShell' -ceq 'powershell' # $False case sensitive equal

'PowerShell' -like '*shell'

'PowerShell' -match '^*.shell$'# RegEx

# 'SQL Sat'; -replace is case insensitive

'SQL Saturday' -replace 'saturday','Sat' 

# 'SQL Saturday'; the method on dotnet string is case sensitive

'SQL Saturday'.Replace('saturday','Sat') 

```

  

### Array

```powershell

$arr = @() # define an empty array

$arr.Count # 0

$arr = @(0, 1, 2, 3)

$arr.Count # 4

$arrComma = 0, 1, 2, 3 # same value as $arr

  

# When the array is on the left side, every item gets compared.

# Instead of returning True, it returns the object that matches.

$arrComma -eq 2 # 2

$arr -ne 2 # 0, 1, 3

  

$arr[0] # 0

$arr[0, 2, 3] # 0, 2, 3

$arr[1..3] # 1, 2, 3

$arr[3..1] # 1, 2, 3

  

$null -eq $arr[9000] # True

  

$arr + $arrComma # concat

$arr += 4 # create a new array in place and add an item

$arr * 3 # copy 3 times

  

# foreach can take a script block, notice no space btw the brace

$arr.foreach{"Item [$PSItem]"} 

$data | Where-Object {$_.FirstName -eq 'Kevin'}

  

$arr -join '-' # '0-1-2-3'

$data = @('ATX-SQL-01','ATX-SQL-02','ATX-SQL-03')

$data -replace 'ATX','LAX' # apply to all elems

  

$data -contains 'green' # test in

'green' -in $data

  

Get-Member -InputObject $arr # members of array

```

  

#### ArrayList (Depricated as not a generic type i.e. raw type)

```powershell

$arrList = [System.Collections.ArrayList]::new()

[void]$myArray.Add('Value') # [void] to suppress the return code

```

  

#### Generic List

```powershell

$mylist = [System.Collections.Generic.List[string]]::new()

$mylist = [System.Collections.Generic.List[int]]@(1,2,3)

  

# or

using namespace System.Collections.Generic

$myList = [List[int]]@(1,2,3)

```

  

### Hash Table

  

```powershell

$ageMap = @{} # empty table

$ageMap.add( 'Alex', 9 )

$ageMap['Jerrie'] = 32

$ageMap['Alex']

  

$ageMap = @{

 Alex = 9

 Jerrie = 32

}

  

$ageMap['Alex', 'Jerre'] # @(9, 32)

$ageMap.keys

$ageMap.GetEnumerator() # iter of key-value pair

$ageMap.ContainsKey('Alex')

  

$person = @{ name = 'kevin'; age = 36; } # inline syntax

  

# ordered map

$person = [ordered]@{

 name = 'Kevin'

 age = 36

}

  

$person = @{

 name = 'Kevin'

 age = 36

}

$person += @{Zip = '78701'}

$person.location = @{}

$person.location.city = 'Austin'

$person.location.state = 'TX'

  
  

# expression

# n short for name and e short for expression

$drives | Select-Object -property name, @{n='totalSpaceGB';e={($_.used + $_.free) / 1GB}}

# sort

Get-ADUser | Sort-Object -Parameter @{ e={ Get-TotalSales $_.Name } }

```

  

#### Splatting like (**kwargs in python)

  

```powershell

Add-DhcpServerv4Scope -Name 'TestNetwork' -StartRange'10.0.0.2' -EndRange '10.0.0.254' -SubnetMask '255.255.255.0' -Description 'Network for testlab A' -LeaseDuration (New-TimeSpan -Days 8) -Type "Both"

  

# equivalent

$Common = @{

 SubnetMask = '255.255.255.0'

 LeaseDuration = (New-TimeSpan -Days 8)

 Type = "Both"

}

  

$DHCPScope = @{

 Name = 'TestNetwork'

 StartRange = '10.0.0.2'

 EndRange = '10.0.0.254'

 Description = 'Network for testlab A'

}

Add-DhcpServerv4Scope @DHCPScope

```

  

### Object

  

> Objects are reference types

  

#### PSCustomObject

  

> The idea behind using a PSCustomObject is to have a simple way to create structured data.

with `PSCustomObject`, the keys as column names.

  

```powershell

$myObject = [pscustomobject]$myHashtable # convert from a hash table

```

  

#### DotNet Object

```powershell

$log = New-Object -TypeName System.Diagnostics.EventLog -ArgumentList Application

```

  

#### COM Object

  

## Control flow

  

```powershell

# foreach loop

foreach ($file in Get-ChildItem) {

 Write-Output "$($file.BaseName).woo"

}

  

# for loop

for ($i = 1; $i -lt 5; $i++) {

 Write-Output "Sleeping for $i seconds"

 Start-Sleep -Seconds $i

}

  

# do while loop

$number = Get-Random -Minimum 1 -Maximum 10

do {

 $guess = Read-Host -Prompt "What's your guess?"

 if ($guess -lt $number) {

 Write-Output 'Too low!'

 }

 elseif ($guess -gt $number) {

 Write-Output 'Too high!'

 }

}

# while ($guess -ne $number)

until ($guess -eq $number)

  

# while

while ($i -lt 5) {

 $i += 1

 if ($i -eq 3) {

 continue # we also have break

 }

 Write-Output $i

}

```

  

## Exception

```powershell

Try {

 # Do something with a file.

} Catch [System.IO.IOException] {

 Write-Host "Something went wrong"

} Catch {

 # Catch all. It's not an IOException but something else.

} Finally {

 # Clean up resources.

}

  

Try {

 If ($Path -eq './forbidden')

 {

 Throw "Path not allowed"

 }

 # Carry on.

  

} Catch {

 Write-Error "$($_.exception.message)" # Path not allowed.

}

```

  

## Function

  
  
  

## Cmdlet

  

PowerShell 命令称为“cmdlet”（读成“command-let”）。

cmdlet 是操纵单个功能的命令。

术语“cmdlet”意在表示“小命令”。

  

Cmdlet 遵循动词-名词命名约定

建议 cmdlet 创建者为每个 cmdlet 随附一个帮助文件。

  

Cmdlet 在模块中提供。 PowerShell 模块是一个 DLL，其中包含用于处理每个可用 cmdlet 的代码。

  

可以使用 `Get-Module` 命令获取已加载的模块列表

`Import-Module`, `Update-Module`

```powershell

Install-Module -Name Az -AllowClobber -SkipPublisherCheck`

```

  

### Parameter

  

see `Backup.ps1`

  

```powershell

Param(

 [Parameter(Mandatory, HelpMessage = "Please provide a valid path")]

 [string]$Path

)

New-Item $Path

Write-Host "File created at path $Path"

```

  

### Pipeline

  

cmdlet parameters may or may not accept pipeline input, the info can be get by

`Get-Help` of the cmdlet.

  

There are 2 types of passing/binding methods

- _ByValue_: more precisely to be by type. The parameter takes the same type of output get binded to the result

- _ByPropertyName_ 

- _ByValue & ByPropertyName_: first try _ByValue_ 

  
  
  

## Provider

  

> A provider in PowerShell is an interface that allows file system like access to a datastore.

  

The actual drives that these providers use to expose their datastore can be determined with the `Get-PSDrive` cmdlet.

  

> PSDrives can be accessed just like a traditional file system.

  

Third-party modules such as the `ActiveDirectory` PowerShell module and the `SQLServer` PowerShell module both add their own PowerShell provider and PSDrive.
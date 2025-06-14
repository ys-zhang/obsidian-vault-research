# 1 Open XML Document Model

The follow is an example of Excel workbook 
```
book1
├── [Content_Types].xml
├── _rels
├── docProps
│   ├── app.xml
│   ├── core.xml
│   └── custom.xml
└── xl
    ├── _rels
    │   └── workbook.xml.rels
    ├── sharedStrings.xml
    ├── styles.xml
    ├── tables
    │   └── table1.xml
    ├── theme
    │   └── theme1.xml
    ├── workbook.xml
    └── worksheets
        ├── _rels
        │   └── sheet2.xml.rels
        ├── sheet1.xml
        └── sheet2.xml
```
- `[Content_Types].xml` contains info of "type"^[simila to XML Schema] of each of the other xml files;
- all other files fall into 2 categories
  1. _part_ file, such as `xl/workbook.xml` contains contents of document.
  2. _relation_ file, such as `xl/_rels/workbook.xml.rels` basically describes how parts are related. The purpose of these files is for speedup IO by avoiding large _parts_. 
      - Example 1, `xl/_rels/workbook.xml.rels` is map of worksheet name to parts containing worksheet data. Data of a worksheets are removed out of `workbook.xml` makes quick r/w of workbook info.
      - Example 2, `xl/worksheets/_rels/sheet2.xml.rels` links `table1.xml` to `sheet2.xml`


# 2 Basic Concepts for Excel

- The _workbook_ serves to bind all the sheets and child objects into an organised single file. The workbook properties include information about what application last saved the file, where and how the windows of the workbook were positioned, and an enumeration of the worksheets in the workbook.
- Sheet information is organised into 3 main sections:
  1. Top-level sheet properties (everything before sheetData);
  2. The cell table (sheetData);
  3. Supporting sheet features (everything after sheetData)
- excel sheets stores cells in _row-major_.

## 2.1 Row

```xml
  <xsd:complexType name="CT_Row">
    <xsd:sequence>
      <xsd:element name="c" type="CT_Cell" 
                   minOccurs="0" maxOccurs="unbounded"/>
      <xsd:element name="extLst" minOccurs="0"
                   type="CT_ExtensionList"/>
    </xsd:sequence>
    <!-- row index number, start from 1 --> 
    <xsd:attribute name="r" type="xsd:unsignedInt"
                   use="optional"/>
    <!-- range of non-empty cells in the row --> 
    <xsd:attribute name="spans" type="ST_CellSpans" 
                   use="optional"/>
    <!-- style index 
      The index of this cell's style. 
      Style records are stored in the Styles Part. 
    --> 
    <xsd:attribute name="s" type="xsd:unsignedInt" 
                   use="optional" default="0"/>
    <xsd:attribute name="customFormat" type="xsd:boolean"
                   use="optional" default="false"/>
    <xsd:attribute name="ht" type="xsd:double" 
                   use="optional"/>
    <xsd:attribute name="hidden" type="xsd:boolean" 
                   use="optional" default="false"/>
    <xsd:attribute name="customHeight" type="xsd:boolean"
                   use="optional" default="false"/>
    <xsd:attribute name="outlineLevel" type="xsd:unsignedByte" 
                   use="optional" default="0"/>
    <xsd:attribute name="collapsed" type="xsd:boolean" 
                   use="optional" default="false"/>
    <xsd:attribute name="thickTop" type="xsd:boolean" 
                   use="optional" default="false"/>
    <xsd:attribute name="thickBot" type="xsd:boolean" 
                   use="optional" default="false"/>
    <xsd:attribute name="ph" type="xsd:boolean" 
                   use="optional" default="false"/>
  </xsd:complexType>
```

## 2.2 Cell

```xml
  <xsd:complexType name="CT_Cell">
    <xsd:sequence>
      <xsd:element name="f" type="CT_CellFormula" 
                   minOccurs="0" maxOccurs="1"/>
      <xsd:element name="v" type="s:ST_Xstring" 
                   minOccurs="0" maxOccurs="1"/>
      <!-- rich inline string -->
      <xsd:element name="is" type="CT_Rst" 
                   minOccurs="0" maxOccurs="1"/>
      <xsd:element name="extLst" type="CT_ExtensionList"
                   minOccurs="0"/>
    </xsd:sequence>
    <!-- location of the cell -->
    <xsd:attribute name="r" type="ST_CellRef" 
                   use="optional"/>
    <xsd:attribute name="s" type="xsd:unsignedInt" 
                   use="optional" default="0"/>
    <xsd:attribute name="t" type="ST_CellType" 
                   use="optional" default="n"/>
    <xsd:attribute name="cm" type="xsd:unsignedInt"
                   use="optional" default="0"/>
    <xsd:attribute name="vm" type="xsd:unsignedInt" 
                   use="optional" default="0"/>
    <xsd:attribute name="ph" type="xsd:boolean" 
                   use="optional" default="false"/>
  </xsd:complexType>

  <xsd:simpleType name="ST_CellType">
    <xsd:restriction base="xsd:string">
      <!-- boolean -->
      <xsd:enumeration value="b"/> 
      <!-- date -->
      <xsd:enumeration value="d"/> 
      <!-- number, this is the default type -->
      <xsd:enumeration value="n"/> 
      <!-- error -->
      <xsd:enumeration value="e"/> 
      <!-- shared str -->
      <xsd:enumeration value="s"/> 
      <!-- formula str, seems to be deprecated -->
      <xsd:enumeration value="str"/> 
      <!-- inline str -->
      <xsd:enumeration value="inlineStr"/>
    </xsd:restriction>
  </xsd:simpleType>

  <xsd:complexType name="CT_Rst">
    <xsd:sequence>
      <xsd:element name="t" type="s:ST_Xstring" 
                   minOccurs="0" maxOccurs="1"/>
      <xsd:element name="r" type="CT_RElt" 
                   minOccurs="0" maxOccurs="unbounded"/>
      <xsd:element name="rPh" type="CT_PhoneticRun"
                   minOccurs="0" maxOccurs="unbounded"/>
      <xsd:element name="phoneticPr" type="CT_PhoneticPr"
                   minOccurs="0" maxOccurs="1" />
    </xsd:sequence>
  </xsd:complexType>

```

- (_shared string table_): String values in a cell are not stored in the cell table unless they are the result of a calculation. Therefore, the cell's `v` node will be a _zero-based index_ into the _shared string table_ where that string is stored uniquely.
- (_styling_): The 'master' cell style record (`<xf>`) ties together all the formatting (e.g. number format, font information, and fill) for a cell's direct formatting. An `<xf>` inside `<cellXfs>` is referenced by _zero based index_ (not ID) (s) from a cell definition (`<c>`) in one of the sheets.^[for detail see _L.2.7_]


## 2.3 Formula

The concept is elaborated in _Annex L.2.2.9.2.1_.

For formula we need to care about two aspects:
1. shared formula;
2. external reference

formula can be shared within a _row_, _column_ or _block_, the top-left most cell is the _master cell_ of the shared formula, which is responsible for storing 
1. declare the formula is shared (`{xml}<f t="shared" ...> ... </f>`)^[all cells besides master cell needs to have attr `t="shared"`]
2. the range of share (`{xml}<f ref="A1:A10"> ... </f>`);
3. the formula to be shared;


```xml
<xsd:complexType name="CT_CellFormula">
  <xsd:simpleContent>
    <xsd:extension base="ST_Formula">
      <xsd:attribute name="t" type="ST_CellFormulaType" 
                     use="optional" default="normal"/>
      <xsd:attribute name="aca" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="ref" type="ST_Ref" 
                     use="optional"/>
      <xsd:attribute name="dt2D" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="dtr" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="del1" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="del2" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="r1" type="ST_CellRef" 
                     use="optional"/>
      <xsd:attribute name="r2" type="ST_CellRef" 
                     use="optional"/>
      <xsd:attribute name="ca" type="xsd:boolean" 
                     use="optional" default="false"/>
      <xsd:attribute name="si" type="xsd:unsignedInt" 
                     use="optional"/>
      <xsd:attribute name="bx" type="xsd:boolean" 
                     use="optional" default="false"/>
    </xsd:extension>
  </xsd:simpleContent>
</xsd:complexType>

<xsd:simpleType name="ST_CellFormulaType">
  <xsd:restriction base="xsd:string">
    <xsd:enumeration value="normal"/>
    <xsd:enumeration value="array"/>
    <!-- a data-table is sth like a sheet-defined-function -->
    <xsd:enumeration value="dataTable"/>
    <!-- shared formula -->
    <xsd:enumeration value="shared"/>
  </xsd:restriction>
</xsd:simpleType>

```

### Shared formula

Note that while formulas can be shared, it is desirable to enable easy access to the contents of a cell. Therefore, _it is allowed that all formulas can be written out, but only the primary formula in a shared formula need be loaded and parsed_.


>[!note] attribute `si`
> Optional attribute to optimise load performance by sharing formulas.  
>
> When a formula is a _shared formula_ (`t` value is `shared`) then this value indicates the group to which this particular cell's formula belongs. The first formula in a group of shared formulas is saved in the `f` element. This is considered the 'master' formula cell. Subsequent cells sharing this formula need not have the formula written in their `f` element. 
>   1. Instead, _the attribute `si` value for a particular cell is used to figure what the formula expression should be based on the cell's relative location to the master formula cell._  
>   2. A cell is shared only when `si` is used and `t` is shared.
>   3. The formula expression for a cell that is specified to be part of a shared formula (and is not the master) shall be ignored, and the master formula shall override.  
>   4. If a master cell of a shared formula range specifies that a particular cell is part of the shared formula range, and that particular cell does not use the `si` and `t` attributes to indicate that it is shared, then the particular cell's formula shall override the shared master formula. 
>   5. If this cell occurs in the middle of a range of shared formula cells, the earlier and later formulas shall continue sharing the master formula, and the cell in question shall not share the formula of the master cell formula.  
>   6. Loading and handling of a cell and formula using an `si` attribute and whose `t` value is shared, located outside the range specified in the master cell associated with the `si` group, is implementation defined.

### External reference

```xml
<c r="B2"> 
  <f>[1]Sheet1!$A$1</f> 
  <v>1</v> 
</c>
```

>[!note]
>Instead of writing `‘C:\[ExternalBook.xlsx]Sheet1’!$A$1` directly in the formula, it is desirable to make all external references much more accessible, especially given the potentially enormous size of a cell table. Therefore, _the URL and file location is persisted using the relationships semantic, in a relationship file, and then referenced inline with the formula: `[1]Sheet1!$A$1`_. In this way, external resource files can more easily be determined and updated if needed.


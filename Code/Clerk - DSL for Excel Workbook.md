#Haskell #DSL 

# 1 Idea

A table in Excel can be seen as a tree.
![[Pasted image 20241024162804.png]]
where 
- leafs are data columns
- intermediate nodes are headers

```haskell
-- ============================================================
-- Basic Data Types
-- ============================================================
type TagForest t a = [TagTree t a]
data TagTree t a = TagNode t (TagForest t a)
                 | TagLeaf a
type ReportTemplate raw cell 
  = TagForest HeaderName (CellTemplate raw cell)
type CellTemplate raw cell = raw -> cell

-- ============================================================
-- Combinators
-- ============================================================

column :: (raw -> cell) -> ReportTemplate raw cell

node   :: HeaderName 
       -> ReportTemplate raw cell 
       -> ReportTemplate raw cell 
       
cat    :: ReportTemplate raw cell 
       -> ReportTemplate raw cell 
       -> ReportTemplate raw cell 

instance Monoid (ReportTemplate raw cell) where
  mempty = []
  (<>) = cat
  
```


## 1.1 Formula and References

```haskell
type CellTemplate r c 
  = Coord -> r -> c 

type ReportBuilder r c a 
  = StateT ReportBuilderState (Writer (ReportTemplate r c)) a

report :: ReportBuilder r c a -> ReportTemplate r c
report = execReportBuilder
 where 
  execReportBuilder = 
    execWriter . flip evalStateT initReportBuilderState
    
-- ============================================================
-- Combinators
-- ============================================================

column :: (Coord -> raw -> cell) 
       -> ReportBuilder raw cell Coord
       -- ^ the returned Coord is the coordinates 
       -- of the column, i.e. column number & 
       -- the row number of the first data cell

node   :: HeaderName 
       -> ReportBuilder raw cell a
       -> ReportBuilder raw cell a
       
-- the new cat is just the monad bind (>>=)

subreport :: (whole -> part)
          -> ReportBuilder part a
          -> ReportBuilder whole a
```

- you can use _recursive do_ to refer to some column that is positioned on the right of the current column
## 1.2 Export

```haskell
type XlsxReportTemplate r = ReportTemplate r XlsxCell
makeXlsxReport :: XlsxReportTemplate r
               -> Text        -- ^ table name :1st row header
               -> ReportTemplate () r  -- ^ data for each row
               -> Xlsx
```

# 2 clerk

The _clerk_ package implements the idea.

```haskell
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Bifunctor (second)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer (execWriter)

type SheetName = T.Text
type Sheet a = StateT SheetState (Writer Transform) a
data SheetState = MkSheetState
  { _sheetWsName :: SheetName
  , _sheetWbPath :: FilePath
  }
$(makeLenses ''SheetState)

-- ===========================================================
-- Transform
-- ===========================================================
type WsTransform = X.Worksheet -> X.Worksheet
type FmTransform = FormatMap -> FormatMap 
type FcTransform = X.FormattedCell -> X.FormattedCell
type FormatMap   = Map.Map (X.RowIndex, X.ColumnIndex)      
                            X.FormattedCell
  
-- | a transform alters values & and formats of 
-- a worksheet
data Transform = MkTransform
  { _fmTransform :: FmTransform 
  , _wsTransform :: WsTransform
  }

instance Semigroup Transform where
  MkTransform f1 s1 <> MkTransform f2 s2 = 
    MkTransform (f2 . f1) (s2 . s1)
instance Monoid Transform where
  mempty = MkTransform id id

-- ===========================================================
-- Xlsx
-- ===========================================================
composeXlsx :: FilePath
            -> [(SheetName, Sheet ())]
            -> X.Xlsx
composeXlsx path xs = 
  let sheet_names = map fst xs
      sheet_trans = map get_trans xs
      workbook    = mk_workbook (sheet_names `zip` sheet_trans)
  in  workbook & apply_global_trans sheet_trans
 where
  mk_workbook :: [(SheetName, Transform)] -> X.Xlxs
  mk_workbook ts = let style  = X.def :: X.StyleSheet
                       sheets = second _fmTransform <$> ts
                   in  X.formatWorkbook sheets style
  get_trans :: (SheetName, Sheet()) -> Transform 
  get_trans (n, builder) = 
    execWriter $ execStateT builder (MkSheetState n path)
  apply_global_trans :: [Transform] 
                     -> X.Xlsx -> X.Xlsx
  apply_global_trans ts = X.xlSheets %~ \sheets ->
    zipWith (\t (name, sheet) -> 
               ( name
               , sheet & _wsTransform t
                       & adjust_column_width 
               )
            ) 
            ts 
            sheets
  adjust_column_width = 
    X.wsColumnsProperties %~ filter (isJust . X.cpWidth)
```

## 2.1 cell:

```haskell
-- | a function that formats a cell (celldata)
-- called `FormatCell` in the source code
type CellFormatter m
  = InputIndex -> Coords -> CellData -> m X.FormattedCell

-- | a cell template is a function 
data CellTemp m i o = CellTemp
  { _mkOutput :: i -> o
  , _fmtCell  :: CellFormatter m
  , _colProp  :: Maybe X.ColumnProperties
  }

data CellData
  = CellFormula X.CellFormula
  | CellValue X.CellValue
  | CellComment X.Comment
  | CellEmpty
  deriving stock (Show)
```

## 2.2 row:

a `RowTemp` is not necessarily a row in a sheet, in most cases it represents a column, and in essence it represents a collection of cells/cell-templates.

```haskell
type RowTemp m i o = [CellTemp m i o]
type RowState = Coords 
-- ^ pos of the 1st cell of the row
type Row m i o a = StateT RowState (Writer (RowTemp m i o)) a
```

## 2.3 column:

```haskell
column :: CellFormatter m -> (i -> o) -> Row m i o (Ref a)
column fmt f = do 
  rs <- get
  let cpMin = rs ^. col . to fromIntegral 
      cpMax = cpMin
      props = defColProps { X.cpMin = cpMin, X.cpMap = cpMax }
  tell $ RowTemp [CellTemp{ _fmtCell = fmt
                          , _mkOutput = f
                          , _colProp = props }]
  ref <- gets MkRef
  modify' (col +~ 1) -- advance the column pointer
  pure ref
 where
  defColProps = X.ColumnProperties
    { cpMin       = 1        -- ^ fst col affected by the prop
    , cpMax       = 1        -- ^ lst col affected by the prop
    , cpWidth     = Nothing
    , cpStyle     = Nothing
    , cpHidden    = False
    , cpCollapsed = False
    , cpBestFit   = False
    }
```

## 2.4 render

```haskell
type TempRender i 
  =  RowState            -- ^ current column pointer
  -> InputIndex 
  -> i 
  -> RowTemp i CellData 
  -> Sheet Transform

-- run temp render with inputs one-by-one
renderInputs :: RowState 
             -> TempRender i 
             -> [i]
             -> (i -> Row m i CellData a)
             -> Sheet (Transform, [a])
```
# 3 References

- [The talk](https://youtu.be/1xGoa-zEOrQ?si=FlLc5N89reSJF9SU)
- [clerk](https://hackage.haskell.org/package/clerk)

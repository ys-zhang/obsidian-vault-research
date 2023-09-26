#data-structure #algorithm #editor


# Piece Table

 > A key characteristic of the piece table is that it records all of the insertions we make to a file in an _append-only_ manner.

```haskell
data PTable = PTable 
    { buffers :: [(Buf, Text)]
    , pieces :: [Piece]
    }

data Buf = Origin -- buffer holding original text
         | Add    -- buffer holding content of edits

data Piece = Piece 
    { src :: Buf
    , start :: Int
    , len :: Int
    }

-- view current document
currDoc :: PTable -> Text 
currDoc PTable{buffers, pieces} = 
    concat ( get_text <$> pcs )
  where 
    get_text Piece{src, start, len} = 
       take len . drop start . fromJust $ lookup src buffers
```

now edits apply to current document will 
1. append to the `Add` buffer
2. modify the `pieces` list

since the `pieces` list much shorter than the original buffer
# Refs

- https://dev.to/_darrenburns/the-piece-table---the-unsung-hero-of-your-text-editor-al8
- [what's been wrought using piece tables](https://web.archive.org/web/20160308183811/http://1017.songtrellisopml.com/whatsbeenwroughtusingpiecetables)
- [rope wiki](https://en.wikipedia.org/wiki/Rope_(data_structure))

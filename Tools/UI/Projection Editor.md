#ui #web 

1. automatic growing width
2. fluent editing
3. sync

 ```haskell
adaptiveInput_ :: Monad m => [Attributes] -> Text -> HtmlT m ()
adaptiveInput_ attrs initVal =
  span_ (class_ "relative" : as)
    do span_ [class_ "whitespace-pre", ariaHidden_ True] 
             (toHtml initVal)
       input_ [ class_ "absolute left-0 w-full outline-none"
              , type_  "text"
              , value_ initVal
              ]

```
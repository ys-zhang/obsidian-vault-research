# Blaze


```haskell
type Html = Markup
type Markup = MarkupM ()

class ToHtml a where
  toHtml :: a -> Html
```

Conceptually, `MarkupM` is a state monad or _monoid accumulator_ where the _state represent the text content written in the markup language_.

```haskell
instance Monad MarkupM
instance IsString (MarkupM ())
instance Monoid a => Monoid (MarkupM a)
instance Attributable (MarkupM a)
instance Attributable (MarkupM a -> MarkupM b)

class Attributable a where
 -- attaching attribute
 (!) :: a -> Attribute -> a

(!?) :: Attributable h => h -> (Bool, Attribute) -> h
```

We have renders to render `Html` to text like types.

```haskell
renderHtml :: Html -> String
```

# Heist

_Heist_ uses _quasi-quotation_ in [[Template Haskell]] 

> Heist templates are HTML (or XML) fragments that are not required to have a single root element.

## The `<bind>` tag 

### Define new tags

```html
<bind tag="longname">
    Einstein, Feynman, Heisenberg, and Newton Research Corporation Ltd.<sup>TM</sup>
</bind>
```

the `<bind tag="longname">` tag defines a new tag `<longname>` that will expand to the children of the bind tag.

```html
<p>
    We at <longname/> have research expertise in many areas of physics.
    Employment at <longname/> carries significant prestige.  The rigorous
    hiring process developed by <longname/> is leading the industry.
</p>
```

### Define new "identifier" for attribute substitution

```html
<bind tag="foo">dynamic_name</bind>
<p name"${foo}">A paragraph</p>
```

## The `<apply>` tag for template including 

```html
<html>
  ... 
  <apply template="callee-template">
    <div> this will pass to callee as `apply-content` tag </div>
  </apply>
  ...
</html>
```

this includes the template in file `callee-template.tpl` at the position of the `apply` tag.

### passing parameter to sub-templates

the _caller_ can pass 2 kind of parameters to the _callee_
1. named parameter, through `<bind>` inside the `<apply>` tag 
2. unnamed parameter, which is all else inside the `<apply>` tag, i.e. anything except `<bind>`, can be referred in the _callee_ by a `<bind>` tag _without `tag` attribute_
3. anything (including `<bind>`) inside the `<apply>` tag, the passed argument can be referred in the _callee_ by `<apply-content>` tag

## The `<ignore>` tag

All `<ignore>` tags and their contents will be eliminated in a template’s output.

## Splice 

Heist lets you bind _tags_ to Haskell code with a splice.

>[!def] splice
>A `Splice` takes the input node from the template and outputs a list of nodes that get “spliced” back into the template.

```haskell
-- | represents actions of compilation, run in monad m
newtype HeistT n m a = HeistT 
  { runHeistT :: Node -> HeistState n -> m (a, HeistState n)  }
 
-- the compiled stuff, represents the render process in monad n
data HeistState n
```


>[!note] runtime, now & load-time
> There is a convention in Heist that _`IO` means load time and `n` means runtime._
>
> The terminology here,
> - _load time_ is the monad in which snap initiate the server states and load files from disk
> - _runtime_ is the monad in which snap process http requests
>
> in the type signature of `HeistT n m a`:
>  - `n` represents the _runtime_, which is used in `HeistState n` 
>  - `m` represents the _now_
> 
> The term _now_ is a monad in which we do some _preprocessing_, in other words, its the _compile time_. Basically, given a `Heist n m a` which represents some action in monad `m`, executing these action in `m` is dubbed as compilation which gives us some `HeistState n` as compiled template; the render process will be run monad `n` using the information of `HeistState n`.
>


```haskell
-- ============================================================
-- Heist.Interpreted
-- ============================================================
-- | a template is a forest of XML nodes
type Template = [Node] 

-- | same as `Snap.Snaplet.Heist.SnapISplice b`
-- when `n = Handler b b`
type Splice n = HeistT n n Template 

-- | bind splice to a name which can be called in template
-- by `${name}` or `<name/>`
bindSplice :: Text -> Splice n -> HeistState n -> HeistState n
```

```haskell
-- ============================================================
-- Heist.Compiled
-- ============================================================
type Splice n = HeistT n IO (DList (Chunk n))

-- the only 3 ways to construct a `Chunk`
yieldPure :: Builder -> DList (Chunk m)
yieldRuntime :: RuntimeSplice m Builder -> DList (Chunk m)
yieldRuntimeEffect :: Monad m => RuntimeSplice m () -> DList (Chunk m)

textSplice :: (a -> Text) -> a -> Builder
xmlNodeSplice :: (a -> [Node]) -> a -> Builder
htmlNodeSplice :: (a -> [Node]) -> a -> Builder


-- ============================================================
-- package xmlhtml: Text.Blaze.Renderer.XmlHtml 
-- ============================================================
renderHtmlNodes :: Text.Blaze.Html.Html -> [Node]
```

![[Snap#Adding splices]]


# References

1. [CHOOSING AN HTML LIBRARY IN HASKELL](https://vrom911.github.io/blog/html-libraries)
2. [Lucid: templating DSL for HTML](https://chrisdone.com/posts/lucid/)
3. [Lucid 2.0: clearer than before](https://chrisdone.com/posts/lucid2/)
#code-reading 

# Api Specification

## Endpoint

An _endpoint_ is a list of _segments_ concatenated by the type level operator `:>` which must be ended with a `Verb` segment.

Segments can be: 
1. _path segment_: lifted constant strings, i.e. `Symbol`,
2. _query segment_, including:
    - `QueryParam :: Symbol -> * -> *`: `QueryParam "from" Table`
    - `QueryParams :: Symbol -> * -> *`: `QueryParams "select" Column`
    - `QueryFlag :: Symbol -> *`: `QueryFlag "desc"`
3. _verb segment_: 
    - `Verb :: k -> Nat -> [*] -> * -> *`,  
    - `type Get = Verb 'GET 200`, `Get '[JSON, PlainText] User`
and [others](https://docs.servant.dev/en/stable/tutorial/ApiType.html#combinators)

## Combine Endpoints

use `:<|>` to combine multiple endpoints to an API
```haskell
data a :<|> b
```

## Nest Api

```haskell
-- nested api
type NestedApi = 
       Capture "userid" Int :> (    Get '[JSON] User 
                               :<|> DeleteNoContent  )
-- equivalent to the flattened api 
type FlattenedApi = 
       ( Capture "userid" Int :> Get '[JSON] User )
  :<|> ( Capture "userid" Int :> DeleteNoContent  )
```


# Interpretations

## Server

### Entrance Point

```haskell
type Server api = ServerT api Handler
-- | The WAI application
type Application = Request 
                 -> (Response -> IO ResponseReceived) 
                 -> IO ResponseReceived

serve :: (HasServer api '[]) 
      => Proxy api 
      -> Server api 
      -> Application

serveWithContext :: (HasServer api ctx, ServerContext ctx)
                 => Proxy api 
                 -> Context ctx 
                 -> Server api 
                 -> Application
```

### Core Architecture

```haskell
class HasServer api ctx where 
  type ServerT api (m :: * -> *) :: *
  route :: Proxy api 
        -> Context ctx 
        -- ^ arbitrary data for routing
        -> Delayed env (Server api)
        -- ^ a `Delayed` is a repr of a handler with
        --   scheduled delayed checks that can tigger 
        --   errors
        -> Router env
  hoistServerWithContext :: Proxy api  -> Proxy ctx 
                         -> (forall x. m x -> n x)
                         -> ServerT api m 
                         -> ServerT api n

-- | a hetrogeneous list type
data Context ctx where 
  EmptyContext :: Context '[]
  (:.) :: x -> Context xs -> Context (x ': xs)
  infixr 5 :.
```


### Implement Server 

```haskell
newtype Handler a = Handler { runHandler :: ExceptT ServerError IO a }

```

### Routing

A `Router` is just a tree of request handlers

```haskell
runRouterEnv :: Router env -> env -> RoutingApplication
type RoutingApplication 
  = Request 
  -> (RouteResult Response -> IO ResponseReceived) 
  -> IO ResponseReceived
type Router env = Router' env RoutingApplication

-- | a tree of request handlers 
data Router' env a 
  = StaticRouter -- static in the sense of paths
      (Map Text (Router' env a)) 
      -- ^ sub routers for subpaths
      [env -> a]
      -- ^ a list of handlers to be tried in order
      --   type `a` will be some `Result` like type 
      --   allowing errors 
  | CaptureRouter
      [CaptureHint]
      -- ^ method to extract and cast
      --   captured dynamic path segments
      (Router' (Text, env) a)
  | RawRouter (env -> a)   -- leaf handler
  ...
```


## Client

```haskell
class RunClient m => HasClient m api where
  type Client (m :: * -> *) (api :: *) :: *
  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api
  hoistClientMonad :: Proxy m
                   -> Proxy api
                   -> (forall x. mon x -> mon' x)
                   -> Client mon  api
                   -> Client mon' api 

class Monad m => RunClient m where
  -- | How to make a request, with an optional list of status codes
  -- to not throw exceptions, for (default: [200..299]).
  runRequestAcceptStatus :: Maybe [Status] -> Request -> m Response
  throwClientError :: ClientError -> m a
```


```haskell
type Client m (Verb method status cts a) ~ m a

type Client m (Capture s a :> api )    ~  a -> Client m api 
type Client m (QueryParam s a :> api ) ~ Maybe a -> Client m api 
-- | multiple api collection
type Client m (a :<|> b) = (Client m a) :<|> (Client m b)
```


request 

```haskell
data RequestF body path = Request
  { requestPath        :: path
  , requestQueryString :: Seq QueryItem
  , requestBody        :: Maybe (body, MediaType)
  , requestAccept      :: Seq MediaType
  , requestHeaders     :: Seq Header
  , requestHttpVersion :: HttpVersion
  , requestMethod      :: Method
  } deriving (Generic, Typeable, Eq, Functor, Foldable, Traversable)
```

implementations

```haskell
import qualified Network.HTTP.Client as Client

newtype ClientM a = ClientM
  { unClientM :: ReaderT ClientEnv (ExceptT ClientError IO) a }

data ClientEnv = ClientEnv
  { manager :: Client.Manager
  -- ^ Network.HTTP.Client
  , baseUrl :: BaseUrl
  , cookieJar :: Maybe (TVar Client.CookieJar)
  , makeClientRequest :: BaseUrl -> Request -> IO Client.Request
  -- ^ this function can be used to customize the creation of @http-client@ requests from @servant@ requests. Default value: 'defaultMakeClientRequest'
  --   Note that:
  --      1. 'makeClientRequest' exists to allow overriding operational semantics e.g. 'responseTimeout' per request,
  --          If you need global modifications, you should use 'managerModifyRequest'
  --      2. the 'cookieJar', if defined, is being applied after 'makeClientRequest' is called.
  }

```


`client api` returns client functions for our _entire_ API, combined with `:<|>`, which we can pattern match on.

```haskell
runClientM :: ClientM a -> ClientEnv -> IO (Either ClientError a)
mkClientEnv :: Manager -> BaseUrl -> ClientEnv
```


# Auth 

```haskell 



```


# References

1. Mestanogullari, A., Hahn, S., Arni, J. K., & Löh, A. (2015). Type-Level Web APIs with Servant: An Exercise in Domain-Specific Generic Programming. _Proceedings of the 11th ACM SIGPLAN Workshop on Generic Programming_, 1–12. [https://doi.org/10.1145/2808098.2808099](https://doi.org/10.1145/2808098.2808099)
2. [Servant Doc (stable)](https://docs.servant.dev/en/stable/index.html)
3. [Servant Auth](https://github.com/haskell-servant/servant/tree/master/servant-auth#readme)

## Module Reddit.Types

#### `RedditS`

``` purescript
type RedditS = Tuple Milliseconds Token
```

#### `REnv`

``` purescript
type REnv e = ReaderT AppInfo (AffReq e)
```

#### `R`

``` purescript
type R e d = StateT RedditS (REnv e) d
```

#### `Responsable`

``` purescript
class Responsable r where
  fromForeign :: Foreign -> Either Error r
```

##### Instances
``` purescript
instance responsableUnit :: Responsable Unit
instance responsableToken :: Responsable Token
instance responsableSubreddit :: Responsable Subreddit
instance responsableCommentThread :: Responsable CommentThread
```

#### `Requestable`

``` purescript
class Requestable s where
  querystring :: s -> String
```

##### Instances
``` purescript
instance requestableRRequest :: (Requestable a) => Requestable (RedditRequest a)
instance requestableString :: Requestable String
instance requestableUnit :: Requestable Unit
```

#### `AppInfo`

``` purescript
type AppInfo = { id :: String, secret :: String, username :: String, password :: String, userAgent :: String }
```

#### `Token`

``` purescript
newtype Token
  = Token { accessToken :: String, tokenType :: String, expiresIn :: Int, scope :: String }
```

##### Instances
``` purescript
instance responsableToken :: Responsable Token
instance tokenIsForeign :: IsForeign Token
instance genericToken :: Generic Token
instance showToken :: Show Token
instance eqToken :: Eq Token
```

#### `RedditRequest`

``` purescript
newtype RedditRequest a
  = RRequest { endpt :: String, method :: Verb, content :: Maybe a }
```

##### Instances
``` purescript
instance requestableRRequest :: (Requestable a) => Requestable (RedditRequest a)
```

#### `Post`

``` purescript
newtype Post
  = Post { domain :: String, subreddit :: String, selftext :: String, id :: String, author :: String, subredditId :: String, isSelf :: Boolean, permalink :: String, name :: String, created :: Int, url :: String, title :: String }
```

##### Instances
``` purescript
instance genericPost :: Generic Post
instance showPost :: Show Post
instance postIsForeign :: IsForeign Post
```

#### `Subreddit`

``` purescript
newtype Subreddit
  = Subreddit (Array Post)
```

##### Instances
``` purescript
instance showSubreddit :: Show Subreddit
instance subredditIsForeign :: IsForeign Subreddit
instance responsableSubreddit :: Responsable Subreddit
```

#### `SrName`

``` purescript
newtype SrName
  = SrName String
```

#### `mkSrName`

``` purescript
mkSrName :: String -> SrName
```

#### `runSrName`

``` purescript
runSrName :: SrName -> String
```

#### `mapSubreddit`

``` purescript
mapSubreddit :: forall a b. (Array Post -> Array Post) -> Subreddit -> Subreddit
```

#### `Comment`

``` purescript
newtype Comment
  = Comment { subredditId :: String, linkId :: String, replies :: Maybe (Array Comment), id :: String, author :: String, parentId :: String, body :: String, subreddit :: String, name :: String, created :: Int }
```

##### Instances
``` purescript
instance commentIsForeign :: IsForeign Comment
instance genericComment :: Generic Comment
instance showComment :: Show Comment
```

#### `CommentThread`

``` purescript
data CommentThread
  = CommentThread Post (Array Comment)
```

##### Instances
``` purescript
instance genericCommentThread :: Generic CommentThread
instance showCommentThread :: Show CommentThread
instance commentThreadIsForeign :: IsForeign CommentThread
instance responsableCommentThread :: Responsable CommentThread
```



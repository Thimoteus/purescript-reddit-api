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
instance responsableForeign :: Responsable Foreign
instance responsableUnit :: Responsable Unit
instance responsableToken :: Responsable Token
instance responsableSubreddit :: Responsable Subreddit
instance responsableComment :: Responsable Comment
instance responsableCommentThread :: Responsable CommentThread
instance responsableStubbyPost :: Responsable StubbyPost
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
instance requestableLinkPost :: Requestable LinkPost
instance requestableSelfPost :: Requestable SelfPost
instance requestableReply :: Requestable Reply
```

#### `AppInfo`

``` purescript
type AppInfo = { id :: String, secret :: String, username :: String, password :: String, userAgent :: String }
```

#### `Token`

``` purescript
newtype Token
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
```

##### Instances
``` purescript
instance showSubreddit :: Show Subreddit
instance subredditIsForeign :: IsForeign Subreddit
instance responsableSubreddit :: Responsable Subreddit
```

#### `runSubreddit`

``` purescript
runSubreddit :: Subreddit -> Array Post
```

#### `SrName`

``` purescript
newtype SrName
  = SrName String
```

#### `runSrName`

``` purescript
runSrName :: SrName -> String
```

#### `Comment`

``` purescript
newtype Comment
```

##### Instances
``` purescript
instance commentIsForeign :: IsForeign Comment
instance genericComment :: Generic Comment
instance showComment :: Show Comment
instance responsableComment :: Responsable Comment
```

#### `CommentThread`

``` purescript
data CommentThread
```

##### Instances
``` purescript
instance genericCommentThread :: Generic CommentThread
instance showCommentThread :: Show CommentThread
instance commentThreadIsForeign :: IsForeign CommentThread
instance responsableCommentThread :: Responsable CommentThread
```

#### `postFromCommentThread`

``` purescript
postFromCommentThread :: CommentThread -> Post
```

#### `commentsFromCommentThread`

``` purescript
commentsFromCommentThread :: CommentThread -> Array Comment
```

#### `LinkPost`

``` purescript
newtype LinkPost
  = LinkPost { resubmit :: Boolean, sendReplies :: Boolean, subreddit :: String, title :: String, url :: String }
```

##### Instances
``` purescript
instance requestableLinkPost :: Requestable LinkPost
```

#### `SelfPostRec`

``` purescript
type SelfPostRec = { sendReplies :: Boolean, subreddit :: String, title :: String, body :: String }
```

#### `SelfPost`

``` purescript
newtype SelfPost
  = SelfPost { sendReplies :: Boolean, subreddit :: String, title :: String, body :: String }
```

##### Instances
``` purescript
instance requestableSelfPost :: Requestable SelfPost
```

#### `runSelfPost`

``` purescript
runSelfPost :: SelfPost -> SelfPostRec
```

#### `StubbyPost`

``` purescript
newtype StubbyPost
```

##### Instances
``` purescript
instance genericStubbyPost :: Generic StubbyPost
instance showStubbyPost :: Show StubbyPost
instance stubbyPostIsForeign :: IsForeign StubbyPost
instance responsableStubbyPost :: Responsable StubbyPost
```

#### `postToStubbyPost`

``` purescript
postToStubbyPost :: Post -> StubbyPost
```

#### `commentThreadToStubbyPost`

``` purescript
commentThreadToStubbyPost :: CommentThread -> StubbyPost
```

#### `ReplyRec`

``` purescript
type ReplyRec = { body :: String, parent :: String }
```

#### `Reply`

``` purescript
newtype Reply
  = Reply { body :: String, parent :: String }
```

##### Instances
``` purescript
instance genericReply :: Generic Reply
instance showReply :: Show Reply
instance requestableReply :: Requestable Reply
```



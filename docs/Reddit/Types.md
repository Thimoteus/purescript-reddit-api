## Module Reddit.Types

#### `RedditS`

``` purescript
type RedditS = Tuple Milliseconds Token
```

The all-important R monad.

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

Class declarations, basic instances.

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
instance requestableStrMapString :: Requestable (StrMap String)
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

Data types

#### `Token`

``` purescript
newtype Token
```

##### Instances
``` purescript
instance responsableToken :: Responsable Token
instance genericToken :: Generic Token
instance showToken :: Show Token
instance eqToken :: Eq Token
instance tokenIsForeign :: IsForeign Token
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

#### `PostRec`

``` purescript
type PostRec = { domain :: String, subreddit :: String, selftext :: String, id :: String, author :: String, subredditId :: String, isSelf :: Boolean, permalink :: String, name :: String, created :: Int, url :: String, title :: String }
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

#### `CommentRec`

``` purescript
type CommentRec = { subredditId :: String, linkId :: String, replies :: Maybe (Array Comment), id :: String, author :: String, parentId :: String, body :: String, subreddit :: String, name :: String, created :: Int }
```

#### `Comment`

``` purescript
newtype Comment
```

##### Instances
``` purescript
instance responsableComment :: Responsable Comment
instance genericComment :: Generic Comment
instance showComment :: Show Comment
instance eqComment :: Eq Comment
instance commentIsForeign :: IsForeign Comment
```

#### `CommentThread`

``` purescript
data CommentThread
```

##### Instances
``` purescript
instance responsableCommentThread :: Responsable CommentThread
instance genericCommentThread :: Generic CommentThread
instance showCommentThread :: Show CommentThread
instance commentThreadIsForeign :: IsForeign CommentThread
```

#### `Subreddit`

``` purescript
newtype Subreddit
```

##### Instances
``` purescript
instance responsableSubreddit :: Responsable Subreddit
instance showSubreddit :: Show Subreddit
instance subredditIsForeign :: IsForeign Subreddit
```

#### `LinkPostRec`

``` purescript
type LinkPostRec = { resubmit :: Boolean, sendReplies :: Boolean, subreddit :: String, title :: String, url :: String }
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

#### `StubbyPostRec`

``` purescript
type StubbyPostRec = { url :: String, id :: String, name :: String }
```

#### `StubbyPost`

``` purescript
newtype StubbyPost
```

##### Instances
``` purescript
instance responsableStubbyPost :: Responsable StubbyPost
instance genericStubbyPost :: Generic StubbyPost
instance showStubbyPost :: Show StubbyPost
instance stubbyPostIsForeign :: IsForeign StubbyPost
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
instance requestableReply :: Requestable Reply
instance genericReply :: Generic Reply
instance showReply :: Show Reply
```

#### `runPost`

``` purescript
runPost :: Post -> PostRec
```

#### `runSubreddit`

``` purescript
runSubreddit :: Subreddit -> Array Post
```

#### `runComment`

``` purescript
runComment :: Comment -> CommentRec
```

#### `runLinkPost`

``` purescript
runLinkPost :: LinkPost -> LinkPostRec
```

#### `runSelfPost`

``` purescript
runSelfPost :: SelfPost -> SelfPostRec
```

#### `runStubbyPost`

``` purescript
runStubbyPost :: StubbyPost -> StubbyPostRec
```

#### `postFromCommentThread`

``` purescript
postFromCommentThread :: CommentThread -> Post
```

#### `commentsFromCommentThread`

``` purescript
commentsFromCommentThread :: CommentThread -> Array Comment
```

#### `postToStubbyPost`

``` purescript
postToStubbyPost :: Post -> StubbyPost
```

#### `commentThreadToStubbyPost`

``` purescript
commentThreadToStubbyPost :: CommentThread -> StubbyPost
```



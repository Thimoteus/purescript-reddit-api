## Module Reddit.Types

#### `AffReq`

``` purescript
type AffReq e = AffReq (now :: Now | e)
```

The all-important R monad.

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

Class declarations, basic instances.

##### Instances
``` purescript
Responsable Foreign
Responsable Unit
Responsable Token
Responsable Subreddit
Responsable Comment
Responsable CommentThread
Responsable StubbyPost
```

#### `Requestable`

``` purescript
class Requestable s where
  querystring :: s -> String
```

##### Instances
``` purescript
Requestable (StrMap String)
(Requestable a) => Requestable (RedditRequest a)
Requestable String
Requestable Unit
Requestable LinkPost
Requestable SelfPost
Requestable Reply
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
Responsable Token
Generic Token
Show Token
Eq Token
IsForeign Token
```

#### `RedditRequest`

``` purescript
newtype RedditRequest a
  = RRequest { endpt :: String, method :: Verb, content :: Maybe a }
```

##### Instances
``` purescript
(Requestable a) => Requestable (RedditRequest a)
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
Generic Post
Show Post
IsForeign Post
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
Responsable Comment
Generic Comment
Show Comment
Eq Comment
IsForeign Comment
```

#### `CommentThread`

``` purescript
data CommentThread
```

##### Instances
``` purescript
Responsable CommentThread
Generic CommentThread
Show CommentThread
IsForeign CommentThread
```

#### `Subreddit`

``` purescript
newtype Subreddit
```

##### Instances
``` purescript
Responsable Subreddit
Show Subreddit
IsForeign Subreddit
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
Requestable LinkPost
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
Requestable SelfPost
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
Responsable StubbyPost
Generic StubbyPost
Show StubbyPost
IsForeign StubbyPost
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
Requestable Reply
Generic Reply
Show Reply
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



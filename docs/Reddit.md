## Module Reddit

#### `runR`

``` purescript
runR :: forall e. R e Unit -> AppInfo -> (Error -> Eff (request :: REQUEST | e) Unit) -> (Unit -> Eff (request :: REQUEST | e) Unit) -> Eff (request :: REQUEST | e) Unit
```

Run an R computation, given login information and error/success callbacks.

#### `launchR`

``` purescript
launchR :: forall e. R e Unit -> AppInfo -> Eff (err :: EXCEPTION, request :: REQUEST | e) Unit
```

Launch an R computation, given login information, discarding errors.

#### `call`

``` purescript
call :: forall s e d. (Responsable d, Requestable s) => RedditRequest s -> R e d
```

The basic method for interacting with Reddit. You most likely want `get`, `get'`, `post`
or `post'` for more control than methods like `subreddit`, `commentThread`, etc.

#### `get`

``` purescript
get :: forall s e d. (Responsable d, Requestable s) => String -> Maybe s -> R e d
```

A convenience method for making `GET` requests. Check the reddit API
documentation for which API calls use `GET`.

#### `get'`

``` purescript
get' :: forall e d. (Responsable d) => String -> R e d
```

Like `get`, but without any options passed.

#### `post`

``` purescript
post :: forall s e d. (Responsable d, Requestable s) => String -> s -> R e d
```

A convenience function for making `POST` requests. If you don't care about
the response, consider using `post'`.

#### `post'`

``` purescript
post' :: forall s e. (Requestable s) => String -> s -> R e Unit
```

#### `subreddit`

``` purescript
subreddit :: forall s e. (Requestable s) => String -> Maybe s -> R e Subreddit
```

Subreddits are newtypes around an array of Posts.

#### `subreddit'`

``` purescript
subreddit' :: forall e. String -> R e Subreddit
```

#### `commentThread`

``` purescript
commentThread :: forall s e. (Requestable s) => Post -> Maybe s -> R e CommentThread
```

CommentThreads are a tuple of a Post and an array of Comments, representing
the selftext post/linkpost and comment tree.

#### `commentThread'`

``` purescript
commentThread' :: forall e. Post -> R e CommentThread
```

#### `commentThreads`

``` purescript
commentThreads :: forall s e. (Requestable s) => Subreddit -> Maybe s -> R e (Array CommentThread)
```

#### `commentThreads'`

``` purescript
commentThreads' :: forall e. Subreddit -> R e (Array CommentThread)
```

#### `submitLinkPost`

``` purescript
submitLinkPost :: forall e. LinkPostRec -> R e StubbyPost
```

#### `submitSelfPost`

``` purescript
submitSelfPost :: forall e. SelfPostRec -> R e StubbyPost
```

#### `reply`

``` purescript
reply :: forall e. ReplyRec -> R e Comment
```

#### `deleteComment`

``` purescript
deleteComment :: forall e. Comment -> R e Unit
```

#### `deletePost`

``` purescript
deletePost :: forall e. StubbyPost -> R e Unit
```

`deletePost` and `editSelfPost` require a StubbyPost instead of a Post
because a StubbyPost is returned immediately when creating a post, and
any Post can be turned into a StubbyPost with `postToStubbyPost`.

#### `editSelfPost`

``` purescript
editSelfPost :: forall e. StubbyPost -> String -> R e Unit
```

#### `editComment`

``` purescript
editComment :: forall e. Comment -> String -> R e Unit
```



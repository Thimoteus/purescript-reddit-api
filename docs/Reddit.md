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

A convenience function for making `GET` requests. The response must be an instance of
the `Responsable` typeclass, and any options passed must be an instance of the
`Requestable` typeclass.

#### `get'`

``` purescript
get' :: forall e d. (Responsable d) => String -> R e d
```

A convenience function when you don't want to provide options.

#### `post`

``` purescript
post :: forall s e d. (Responsable d, Requestable s) => String -> s -> R e d
```

#### `post'`

``` purescript
post' :: forall s e. (Requestable s) => String -> s -> R e Unit
```

A convenience function for when you don't care about the response.

#### `subreddit`

``` purescript
subreddit :: forall s e. (Requestable s) => String -> Maybe s -> R e Subreddit
```

Given a subreddit name, attempt to get the front page of that subreddit.

#### `subreddit'`

``` purescript
subreddit' :: forall e. String -> R e Subreddit
```

A convenience function for when you don't want to provide options.

#### `commentThread`

``` purescript
commentThread :: forall s e. (Requestable s) => Post -> Maybe s -> R e CommentThread
```

Given a Post, attempt to return the CommentThread of that Post.

#### `commentThread'`

``` purescript
commentThread' :: forall e. Post -> R e CommentThread
```

Gets a CommentThread without extra options.



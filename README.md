#purescript-reddit-api

##Installing

`pulp dep install Thimoteus/purescript-reddit-api`

##Usage

Everything runs in the `R` monad, which is just a monad transformer stack around
`StateT`, `ReaderT` and `Aff`. Rate-limiting and logging in via OAuth are
handled automatically.

As an example:

```purescript
main = runR myRedditBot testAppInfo print return

myRedditBot :: forall e. R e Unit
myRedditBot = do
  ps <- subreddit' "purescript"
  liftEff $ print ps
```

`testAppInfo` is a record with `id`, `secret`, `username`, `password` and
`userAgent` fields, all of which are `String`s.

There are two methods for running a computation in the `R` monad: `launchR` and
`runR`, which correspond to `launchAff` and `runAff` in the expected way.

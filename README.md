#purescript-reddit-api

##Installing

`pulp dep install Thimoteus/purescript-reddit-api`

##Usage

As an example:

```purescript
main = do
  token <- getToken appInfo
  posts <- take 1 <$> subreddit token "test"
  void $ traverse (comment "this is a test") posts
```

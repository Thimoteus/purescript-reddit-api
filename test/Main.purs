module Test.Main where

import Prelude

import Reddit
import Reddit.Types
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class

testAppInfo = { id: ""
              , secret: ""
              , username: ""
              , password: ""
              , userAgent: "" }

main = runR myRedditBot testAppInfo print return

myRedditBot :: forall e. R e Unit
myRedditBot = do
  ps <- subreddit' "purescript"
  liftEff $ print ps

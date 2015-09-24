module Test.Main where

import Prelude

import Reddit
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Aff

foreign import logAnything :: forall a e. a -> Eff (console :: CONSOLE | e) Unit

testAppInfo = { id: "app id"
              , secret: "app secret"
              , username: "bot username"
              , password: "bot password" }

main = launchAff do
  token <- getToken testAppInfo
  logAnything token

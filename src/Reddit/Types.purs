module Reddit.Types where

import Prelude

import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Generic

type AppInfo = { id :: String
               , secret :: String
               , username :: String
               , password :: String
               , userAgent :: String }

foreign import qsify :: forall a. a -> String

newtype Token = Token { accessToken :: String
                      , tokenType :: String
                      , expiresIn :: Int
                      , scope :: String }

instance tokenIsForeign :: IsForeign Token where
  read value = do
    accessToken <- readProp "access_token" value
    tokenType <- readProp "token_type" value
    expiresIn <- readProp "expires_in" value
    scope <- readProp "scope" value
    return $ Token { accessToken: accessToken
                   , tokenType: tokenType
                   , expiresIn: expiresIn
                   , scope: scope }

derive instance genericToken :: Generic Token

instance showToken :: Show Token where
  show = gShow

emptyToken :: Token
emptyToken = Token { accessToken: "", tokenType: "", expiresIn: 0, scope: "" }

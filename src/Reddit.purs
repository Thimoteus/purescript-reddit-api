module Reddit where

import Prelude

import Node.SimpleRequest (
    AffReq(), Opts(), SRHeaderOptions(), Verb(..), srHeaderOpts,
    headers, hostname, path, method, auth
  )
import Node.SimpleRequest.Secure (request)
import qualified Network.HTTP as HTTP
import Data.Tuple (Tuple(..))
import Data.Options ((:=))
import Data.Either (either)
import Data.Foreign.Class (readJSON)

import Reddit.Types
import Reddit.Util

getToken :: forall e. AppInfo -> AffReq e (Tuple String Token)
getToken appinfo = getToken' where

  getToken' = do
    res <- request opts msg
    return $ Tuple appinfo.userAgent
                 $ either (const emptyToken) id $ readJSON $ unsafeToString res.body

  msg :: String
  msg = qsify { grant_type: "password"
              , username: appinfo.username
              , password: appinfo.password }

  opts :: Opts
  opts = headers := optsHeader
      <> hostname := "ssl.reddit.com"
      <> path := "/api/v1/access_token"
      <> method := POST
      <> auth := (appinfo.id ++ ":" ++ appinfo.secret)

  optsHeader :: SRHeaderOptions
  optsHeader =
    srHeaderOpts [ Tuple HTTP.ContentType "application/x-www-form-urlencoded"
                 , Tuple HTTP.UserAgent appinfo.userAgent ]

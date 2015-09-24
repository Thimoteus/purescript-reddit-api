module Reddit where

import Prelude

import Node.SimpleRequest (
    AffReq(), Opts(), SRHeaderOptions(), Verb(..), request, srHeaderOpts,
    headers, hostname, path, method, auth
  )
import qualified Network.HTTP as HTTP
import Data.Tuple (Tuple(..))
import Data.Options ((:=))
import Data.Foreign (Foreign())

import Reddit.Types

getToken :: forall e. AppInfo -> AffReq e (Tuple Foreign String)
getToken appinfo = getToken' where

  getToken' = do
    res <- request opts msg
    return $ Tuple res.body appinfo.userAgent

  msg :: String
  msg = qsify { grant_type: "password"
              , username: appinfo.username
              , password: appinfo.password }

  opts :: Opts
  opts = headers := optsHeader
      <> hostname := "https://ssl.reddit.com"
      <> path := "/api/v1/access_token"
      <> method := POST
      <> auth := (appinfo.id ++ ":" ++ appinfo.secret)

  optsHeader :: SRHeaderOptions
  optsHeader =
    srHeaderOpts [ Tuple HTTP.ContentType "application/x-www-form-urlencoded"
                 , Tuple HTTP.UserAgent appinfo.userAgent ]

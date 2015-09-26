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
import Data.Maybe (maybe, Maybe(..))

import Reddit.Types
import Reddit.Util

-- | Get an oauth token from Reddit.
-- | Returns a (UserAgent, Token) tuple.
getToken :: forall e. AppInfo -> AffReq e UAToken
getToken appinfo = getToken' where

  getToken' = do
    res <- request opts msg
    return $ Tuple appinfo.userAgent $ fromForeign res.body

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

call :: forall s e r. (Responsable r, Requestable s) => UAToken -> RedditRequest s -> AffReq e r
call (Tuple ua (Token t)) (RRequest r) = call' where

  call' = do
    res <- request opts msg
    return $ fromForeign res.body

  msg :: String
  msg = maybe "" querystring r.content

  opts :: Opts
  opts = headers := optsHeader
      <> hostname := "oauth.reddit.com"
      <> path := r.endpt
      <> method := r.method

  optsHeader :: SRHeaderOptions
  optsHeader =
    srHeaderOpts [ Tuple HTTP.UserAgent ua
                 , Tuple HTTP.ContentType "application/x-www-form-urlencoded"
                 , Tuple HTTP.Authorization $ "bearer " ++ t.accessToken ]

get :: forall s e r. (Requestable s, Responsable r) => UAToken -> String -> Maybe s -> AffReq e r
get t endpt s = call t $ RRequest { endpt: endpt ++ ".json"
                                  , method: GET
                                  , content: s }

get' :: forall e r. (Responsable r) => UAToken -> String -> AffReq e r
get' t endpt = get t endpt (Nothing :: Maybe String)

post :: forall s e r. (Requestable s, Responsable r) => UAToken -> String -> s -> AffReq e r
post t endpt content = call t $ RRequest { endpt: endpt
                                         , method: POST
                                         , content: Just content }

post' :: forall s e. (Requestable s) => UAToken -> String -> s -> AffReq e Unit
post' = post

subreddit :: forall s e. (Requestable s) => UAToken -> String -> Maybe s -> AffReq e Subreddit
subreddit t sub s = get t (subbify sub) s

subreddit' :: forall e. UAToken -> String -> AffReq e Subreddit
subreddit' t sub = get' t $ subbify sub

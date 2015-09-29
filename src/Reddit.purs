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
import Data.Either (either)

import Reddit.Types
import Reddit.Util

import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (print)

-- | Get an oauth token from Reddit.
-- | Returns a (UserAgent, Token) tuple.
getToken :: forall e. AppInfo -> AffReq e UAToken
getToken appinfo = getToken' where

  getToken' = do
    res <- request opts msg
    return $ Tuple appinfo.userAgent $ either (const emptyToken) id $ fromForeign res.body

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

-- | The basic method for interacting with Reddit. You most likely want `get`, `get'`, `post`
-- | or `post'` for more control than methods like `subreddit`, `commentThread`, etc.
call :: forall s r. (Responsable r, Requestable s) => UAToken -> RedditRequest s -> AffReddit r
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

-- | A convenience function for making `GET` requests. The response must be an instance of
-- | the `Responsable` typeclass, and any options passed must be an instance of the
-- | `Requestable` typeclass.
get :: forall s r. (Requestable s, Responsable r) => UAToken -> String -> Maybe s -> AffReddit r
get t endpt opts = call t $ RRequest { endpt: endpt ++ ".json"
                                  , method: GET
                                  , content: opts }

-- | A convenience function when you don't want to provide options.
get' :: forall r. (Responsable r) => UAToken -> String -> AffReddit r
get' t endpt = get t endpt (Nothing :: Maybe String)

-- | A convenience function for making `POST` requests. If you care about the response,
-- | it must implement the `Responsable` typeclass.
post :: forall s r. (Requestable s, Responsable r) => UAToken -> String -> s -> AffReddit r
post t endpt content = call t $ RRequest { endpt: endpt
                                         , method: POST
                                         , content: Just content }

-- | A convenience function for when you don't care about the response.
post' :: forall s. (Requestable s) => UAToken -> String -> s -> AffReddit Unit
post' = post

-- | Given a subreddit name, attempt to get the front page of that subreddit.
subreddit :: forall s. (Requestable s) => UAToken -> SrName -> Maybe s -> AffReddit Subreddit
subreddit t sub opts = get t (runSrName sub) opts

-- | A convenience function for when you don't want to provide options.
subreddit' :: UAToken -> SrName -> AffReddit Subreddit
subreddit' t sub = get' t $ runSrName sub

-- | Given a Post, attempt to return the CommentThread of that Post.
commentThread :: forall s. (Requestable s) => UAToken -> Post -> Maybe s -> AffReddit CommentThread
commentThread t (Post o) opts = get t endpt opts where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id

commentThread' :: UAToken -> Post -> AffReddit CommentThread
commentThread' t (Post o) = get' t endpt where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id

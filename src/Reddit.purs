module Reddit (
  call, get, get', post, post',
  subreddit, subreddit', commentThread, commentThread',
  runR
  ) where

import Prelude

import Node.SimpleRequest (
    REQUEST(), AffReq(), Opts(), SRHeaderOptions(), Verb(..), Response(),
    srHeaderOpts, headers, hostname, path, method, auth
  )
import Node.SimpleRequest.Secure (request)
import qualified Network.HTTP as HTTP

import Data.Tuple (Tuple(..), fst, snd)
import Data.Options ((:=))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Data.Date (nowEpochMilliseconds)
import Data.Time (Milliseconds(..))

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), Error())
--import Control.Monad.Eff.Console (print)
import Control.Monad.Aff (attempt, later', launchAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import qualified Control.Monad.State.Class as State -- (get, put)
import qualified Control.Monad.Reader.Class as Env -- (ask)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Apply ((*>))

import Reddit.Types
import Reddit.Util

-- | Run an R computation, given login information.
runR :: forall e. R e Unit -> AppInfo -> Eff ( err :: EXCEPTION, request :: REQUEST | e ) Unit
runR r app = unravelR' where
  unravelR' = launchAff unravelReader
  unravelReader = runReaderT getEnv app
  getEnv = getToken app >>= unravelState
  unravelState = evalStateT unravelExcept
  unravelExcept = runExceptT r

-- | Get an oauth token from Reddit. Returns a Token.
getToken :: forall e. AppInfo -> REnv e RedditS
getToken appinfo = getToken' where

  getToken' = do
    res <- liftAff $ request opts msg
    now <- liftEff nowEpochMilliseconds
    return $ either (const initialState) (Tuple now) $ fromForeign res.body

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

  initialState :: RedditS
  initialState = Tuple (Milliseconds 0.0) (Token { accessToken: ""
                                                 , tokenType: ""
                                                 , expiresIn: 0
                                                 , scope: "" })

-- | The basic method for interacting with Reddit. You most likely want `get`, `get'`, `post`
-- | or `post'` for more control than methods like `subreddit`, `commentThread`, etc.
call :: forall s e d. (Responsable d, Requestable s) => RedditRequest s -> R e d
call req@(RRequest r) = call' where

  call' = do
    st <- State.get
    let past = fst st
        token = snd st
    now <- liftEff nowEpochMilliseconds
    if now - past < Milliseconds 1000.0
      then waitAndCall (closeEnough $ now - past) req
      else continueCall $ Tuple now token

  waitAndCall w z = liftAff (later' w $ pure unit :: AffReq e Unit) *> call z

  continueCall w = do
    State.put w
    appInfo <- Env.ask
    res <- liftAff $ attempt $ request (opts appInfo) msg :: AffReq e Response
    either throwError (either throwError return <<< fromForeign <<< _.body) res
      where
      msg :: String
      msg = maybe "" querystring r.content
      opts :: AppInfo -> Opts
      opts appInfo = headers := optsHeader appInfo
                  <> hostname := "oauth.reddit.com"
                  <> path := r.endpt
                  <> method := r.method
      optsHeader :: AppInfo -> SRHeaderOptions
      optsHeader appInfo = srHeaderOpts
        [ Tuple HTTP.UserAgent appInfo.userAgent
        , Tuple HTTP.ContentType "application/x-www-form-urlencoded"
        , Tuple HTTP.Authorization $ append "bearer " $ _.accessToken $ runToken $ snd w ]

-- | A convenience function for making `GET` requests. The response must be an instance of
-- | the `Responsable` typeclass, and any options passed must be an instance of the
-- | `Requestable` typeclass.
get :: forall s e d. (Responsable d, Requestable s) => String -> Maybe s -> R e d
get endpt opts = call $ RRequest { endpt: endpt ++ ".json"
                                 , method: GET
                                 , content: opts }

-- | A convenience function when you don't want to provide options.
get' :: forall e d. (Responsable d) => String -> R e d
get' endpt = get endpt $ Just unit --(Nothing :: Maybe Unit)

-- | A convenience function for making `POST` requests. If you care about the response,
-- | it must implement the `Responsable` typeclass.
post :: forall s e d. (Responsable d, Requestable s) => String -> s -> R e d
post endpt content = call $ RRequest { endpt: endpt
                                     , method: POST
                                     , content: Just content }

-- | A convenience function for when you don't care about the response.
post' :: forall s e. (Requestable s) => String -> s -> R e Unit
post' = post

-- | Given a subreddit name, attempt to get the front page of that subreddit.
subreddit :: forall s e. (Requestable s) => SrName -> Maybe s -> R e Subreddit
subreddit sub opts = get (runSrName sub) opts

-- | A convenience function for when you don't want to provide options.
subreddit' :: forall e. SrName -> R e Subreddit
subreddit' sub = get' $ runSrName sub

-- | Given a Post, attempt to return the CommentThread of that Post.
commentThread :: forall s e. (Requestable s) => Post -> Maybe s -> R e CommentThread
commentThread (Post o) opts = get endpt opts where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id

commentThread' :: forall e. Post -> R e CommentThread
commentThread' (Post o) = get' endpt where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id

module Reddit (
  runR,
  launchR,
  call,
  get,
  get',
  post,
  post',
  subreddit,
  subreddit',
  commentThread,
  commentThread',
  commentThreads,
  commentThreads',
  submitLinkPost,
  submitSelfPost,
  reply,
  deleteComment,
  deletePost,
  editSelfPost,
  editComment
  ) where

import Prelude

import Node.SimpleRequest (
    REQUEST(), Opts(), SRHeaderOptions(), Verb(..), Response(),
    srHeaderOpts, headers, hostname, path, method, auth
  )
import Node.SimpleRequest.Secure (request)
import qualified Network.HTTP as HTTP

import Data.Tuple (Tuple(..), fst, snd)
import Data.List (List(..), (:))
import Data.StrMap (singleton, fromList)
import Data.Options ((:=))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Date (nowEpochMilliseconds, Now())
import Data.Time (Milliseconds(..))
import Data.Traversable (traverse)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), Error())
import Control.Monad.Aff (attempt, later', launchAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import qualified Control.Monad.State.Class (get, put) as State
import qualified Control.Monad.Reader.Class (ask) as Env
import Control.Monad.Trans (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Apply ((*>))

import Reddit.Types
import Reddit.Util

-- | Run an R computation, given login information and error/success callbacks.
runR :: forall e. R e Unit
               -> AppInfo
               -> (Error -> Eff ( now :: Now, request :: REQUEST | e ) Unit)
               -> (Unit -> Eff ( now :: Now, request :: REQUEST | e ) Unit)
               -> Eff ( now :: Now, request :: REQUEST | e ) Unit
runR r app err succ = runAff err succ $ unravelR r app

-- | Launch an R computation, given login information, discarding errors.
launchR :: forall e. R e Unit -> AppInfo -> Eff ( now :: Now, err :: EXCEPTION, request :: REQUEST | e ) Unit
launchR r = launchAff <<< unravelR r

-- | Unravels the monad transformer stack. The StateT transformer is initialized
-- | with an authentication token grabbed from Reddit.
unravelR :: forall e. R e Unit -> AppInfo -> AffReq e Unit
unravelR r app = unravelR' where
  unravelR' = unravelReader
  unravelReader = runReaderT getEnv app
  getEnv = getToken app >>= unravelState
  unravelState = evalStateT r

login :: forall e. R e Unit
login = do
  appInfo <- Env.ask
  st <- lift $ getToken appInfo
  State.put st

getToken :: forall e. AppInfo -> REnv e RedditS
getToken appinfo = getToken' where

  getToken' = do
    res <- liftAff $ request opts msg
    now <- liftEff nowEpochMilliseconds
    either throwError (return <<< Tuple now) $ fromForeign res.body

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
    appInfo <- Env.ask
    res <- liftAff $ attempt $ request (opts appInfo) msg :: AffReq e Response
    either throwError parseStatusCode res
      where
      parseStatusCode res = case unsafeToInt res.statusCode of
                                 401 -> login *> State.get >>= continueCall
                                 403 -> login *> State.get >>= continueCall
                                 _ -> either throwError return $ fromForeign res.body
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

-- | A convenience method for making `GET` requests. Check the reddit API
-- | documentation for which API calls use `GET`.
get :: forall s e d. (Responsable d, Requestable s) => String -> Maybe s -> R e d
get endpt opts = call $ RRequest { endpt: endpt ++ ".json"
                                 , method: GET
                                 , content: opts }

-- | Like `get`, but without any options passed.
get' :: forall e d. (Responsable d) => String -> R e d
get' endpt = get endpt $ Just unit

-- | A convenience function for making `POST` requests. If you don't care about
-- | the response, consider using `post'`.
post :: forall s e d. (Responsable d, Requestable s) => String -> s -> R e d
post endpt content = call $ RRequest { endpt: endpt
                                     , method: POST
                                     , content: Just content }

post' :: forall s e. (Requestable s) => String -> s -> R e Unit
post' = post

-- | Subreddits are newtypes around an array of Posts.
subreddit :: forall s e. (Requestable s) => String -> Maybe s -> R e Subreddit
subreddit sub opts = get (subbify sub) opts

subreddit' :: forall e. String -> R e Subreddit
subreddit' sub = get' $ subbify sub

-- | CommentThreads are a tuple of a Post and an array of Comments, representing
-- | the selftext post/linkpost and comment tree.
commentThread :: forall s e. (Requestable s) => Post -> Maybe s -> R e CommentThread
commentThread p opts = get endpt opts where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id
  o = runPost p

commentThread' :: forall e. Post -> R e CommentThread
commentThread' p = get' endpt where
  endpt = subbify o.subreddit ++ "/comments/" ++ o.id
  o = runPost p

commentThreads :: forall s e. (Requestable s) => Subreddit -> Maybe s -> R e (Array CommentThread)
commentThreads sr opts = traverse (flip commentThread opts) (runSubreddit sr)

commentThreads' :: forall e. Subreddit -> R e (Array CommentThread)
commentThreads' sr = traverse commentThread' (runSubreddit sr)

submitLinkPost :: forall e. LinkPostRec -> R e StubbyPost
submitLinkPost = post "/api/submit" <<< LinkPost

submitSelfPost :: forall e. SelfPostRec -> R e StubbyPost
submitSelfPost = post "/api/submit" <<< SelfPost

reply :: forall e. ReplyRec -> R e Comment
reply = post "/api/comment" <<< Reply

deleteComment :: forall e. Comment -> R e Unit
deleteComment c = post "/api/del" $ singleton "id" $ _.name $ runComment c

-- | `deletePost` and `editSelfPost` require a StubbyPost instead of a Post
-- | because a StubbyPost is returned immediately when creating a post, and
-- | any Post can be turned into a StubbyPost with `postToStubbyPost`.
deletePost :: forall e. StubbyPost -> R e Unit
deletePost p = post "/api/del" $ singleton "id" $ _.name $ runStubbyPost p

editSelfPost :: forall e. StubbyPost -> String -> R e Unit
editSelfPost p msg =
  let opts = fromList $ Tuple "api_type" "json"
                      : Tuple "text" msg
                      : Tuple "thing_id" (_.name $ runStubbyPost p)
                      : Nil
   in post "/api/editusertext" opts

editComment :: forall e. Comment -> String -> R e Unit
editComment c msg =
  let opts = fromList $ Tuple "api_type" "json"
                      : Tuple "text" msg
                      : Tuple "thing_id" (_.name $ runComment c)
                      : Nil
   in post "/api/editusertext" opts

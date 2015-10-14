module Reddit.Types (
  Responsable,
  Requestable,
  RedditS(),
  REnv(),
  R(),
  AppInfo(),
  Token(),
  PostRec(),
  Post(),
  Subreddit(),
  CommentRec(),
  Comment(),
  CommentThread(),
  StubbyPostRec(),
  StubbyPost(),
  RedditRequest(..),
  SrName(..),
  LinkPostRec(),
  LinkPost(..),
  SelfPostRec(),
  SelfPost(..),
  ReplyRec(),
  Reply(..),
  fromForeign,
  querystring,
  runPost,
  runToken,
  runSubreddit,
  runSrName,
  runComment,
  runLinkPost,
  runSelfPost,
  runStubbyPost,
  commentThreadToStubbyPost,
  postFromCommentThread,
  commentsFromCommentThread,
  postToStubbyPost
  ) where

import Prelude

import Data.Foreign (Foreign(), F())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Generic (Generic, gShow, gEq)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Foreign (readArray, isArray, readString)
import Data.Foreign.Class (readJSON)
import Data.Foreign.Index (prop, hasProperty)
import Data.Traversable (sequence)
import Data.Array (take, filter)
import Data.Array.Unsafe (unsafeIndex)
import Data.Time (Milliseconds())

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), Error(), error)
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Reader.Trans (ReaderT(..))

import Node.SimpleRequest (REQUEST(), Verb(..), AffReq())

import Reddit.Util

type RedditS = Tuple Milliseconds Token
type REnv e = ReaderT AppInfo (AffReq e)
type R e d = StateT RedditS (REnv e) d

class Responsable r where
  fromForeign :: Foreign -> Either Error r

class Requestable s where
  querystring :: s -> String

instance responsableForeign :: Responsable Foreign where
  fromForeign = Right

instance responsableUnit :: Responsable Unit where
  fromForeign = const $ Right unit

type AppInfo = { id :: String
               , secret :: String
               , username :: String
               , password :: String
               , userAgent :: String }

newtype Token = Token { accessToken :: String
                      , tokenType :: String
                      , expiresIn :: Int
                      , scope :: String }

runToken (Token t) = t

instance responsableToken :: Responsable Token where
  fromForeign = either (Left <<< error <<< show) Right <<< readJSON <<< unsafeToString

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

instance eqToken :: Eq Token where
  eq = gEq

type RedditRequestRec a = { endpt :: String, method :: Verb, content :: Maybe a}
newtype RedditRequest a = RRequest { endpt :: String
                                   , method :: Verb
                                   , content :: Maybe a }

runRedditRequest :: forall a. RedditRequest a -> RedditRequestRec a
runRedditRequest (RRequest r) = r

instance requestableRRequest :: (Requestable a) => Requestable (RedditRequest a) where
  querystring = qsify

instance requestableString :: Requestable String where
  querystring = id

instance requestableUnit :: Requestable Unit where
  querystring _ = ""

type PostRec = { domain :: String
               , subreddit :: String
               , selftext :: String
               , id :: String
               , author :: String
               , subredditId :: String
               , isSelf :: Boolean
               , permalink :: String
               , name :: String
               , created :: Int
               , url :: String
               , title :: String }
newtype Post = Post { domain :: String
                    , subreddit :: String
                    , selftext :: String
                    , id :: String
                    , author :: String
                    , subredditId :: String
                    , isSelf :: Boolean
                    , permalink :: String
                    , name :: String
                    , created :: Int
                    , url :: String
                    , title :: String }

runPost :: Post -> PostRec
runPost (Post p) = p

derive instance genericPost :: Generic Post

instance showPost :: Show Post where
  show = gShow

instance postIsForeign :: IsForeign Post where
  read value = do
    domain <- readProp "domain" value
    subreddit <- readProp "subreddit" value
    selftext <- readProp "selftext" value
    _id <- readProp "id" value
    author <- readProp "author" value
    subreddit_id <- readProp "subreddit_id" value
    is_self <- readProp "is_self" value
    permalink <- readProp "permalink" value
    name <- readProp "name" value
    created <- readProp "created" value
    url <- readProp "url" value
    title <- readProp "title" value
    return $ Post { domain: domain
                  , subreddit: subreddit
                  , selftext: selftext
                  , id: _id
                  , author: author
                  , subredditId: subreddit_id
                  , isSelf: is_self
                  , permalink: permalink
                  , name: name
                  , created: created
                  , url: url
                  , title: title }

newtype Subreddit = Subreddit (Array Post)

runSubreddit :: Subreddit -> Array Post
runSubreddit (Subreddit arr) = arr

newtype SrName = SrName String

runSrName :: SrName -> String
runSrName (SrName s) = s

instance showSubreddit :: Show Subreddit where
  show (Subreddit arr) = "Subreddit " ++ show arr

instance subredditIsForeign :: IsForeign Subreddit where
  read value = do
    children <- value # (prop "data" >=> prop "children" >=> readArray)
    arr <- sequence $ map (readProp "data") children
    return $ Subreddit arr

instance responsableSubreddit :: Responsable Subreddit where
  fromForeign = either (Left <<< error <<< show) Right <<< readJSON <<< unsafeToString

type CommentRec = { subredditId :: String
                  , linkId :: String
                  , replies :: Maybe (Array Comment)
                  , id :: String
                  , author :: String
                  , parentId :: String
                  , body :: String
                  , subreddit :: String
                  , name :: String
                  , created :: Int }
newtype Comment = Comment { subredditId :: String
                          , linkId :: String
                          , replies :: Maybe (Array Comment)
                          , id :: String
                          , author :: String
                          , parentId :: String
                          , body :: String
                          , subreddit :: String
                          , name :: String
                          , created :: Int }

runComment :: Comment -> CommentRec
runComment (Comment o) = o

instance commentIsForeign :: IsForeign Comment where
  read value = if hasProperty "json" value
                  then badComment value
                  else goodComment value

badComment :: Foreign -> F Comment
badComment value =
  flip unsafeIndex 0 <$> (prop "json" >=> prop "data" >=> prop "things" >=> readArray) value >>= goodComment

goodComment :: Foreign -> F Comment
goodComment value = do
    c <- prop "data" value
    subreddit_id <- readProp "subreddit_id" c
    link_id <- readProp "link_id" c
    id <- readProp "id" c
    author <- readProp "author" c
    parent_id <- readProp "parent_id" c
    body <- readProp "body" c
    subreddit <- readProp "subreddit" c
    name <- readProp "name" c
    created <- readProp "created" c
    reps <- prop "replies" c
    replies <- if isArray reps
        then Just <$> readProp "replies" c
        else return Nothing
    return $ Comment { subredditId: subreddit_id
                     , linkId: link_id
                     , id: id
                     , author: author
                     , parentId: parent_id
                     , body: body
                     , subreddit: subreddit
                     , name: name
                     , created: created
                     , replies: replies }

derive instance genericComment :: Generic Comment

instance showComment :: Show Comment where
  show = gShow

instance responsableComment :: Responsable Comment where
  fromForeign = either (Left <<< error <<< show) Right <<< readJSON <<< unsafeToString

data CommentThread = CommentThread Post (Array Comment)

postFromCommentThread :: CommentThread -> Post
postFromCommentThread (CommentThread p _) = p

commentsFromCommentThread :: CommentThread -> Array Comment
commentsFromCommentThread (CommentThread _ arr) = arr

derive instance genericCommentThread :: Generic CommentThread

instance showCommentThread :: Show CommentThread where
  show = gShow

instance commentThreadIsForeign :: IsForeign CommentThread where
  read value = do
    tree <- readArray value
    let parent = unsafeIndex tree 0
        children = unsafeIndex tree 1
    post <- (flip unsafeIndex 0 <$> ((prop "data" >=> prop "children" >=> readArray) parent)) >>= readProp "data"
    commentsArr <- children # (prop "data" >=> prop "children" >=> readArray)
    comments <- sequence $ map read $ filter isComment commentsArr
    return $ CommentThread post comments
      where
        isComment :: Foreign -> Boolean
        isComment f = either (const false) (== "t1") $ prop "kind" f >>= readString

instance responsableCommentThread :: Responsable CommentThread where
  fromForeign = either (Left <<< error <<< show) Right <<< readJSON <<< unsafeToString

type LinkPostRec = { resubmit :: Boolean
                   , sendReplies :: Boolean
                   , subreddit :: String
                   , title :: String
                   , url :: String }
newtype LinkPost = LinkPost { resubmit :: Boolean
                            , sendReplies :: Boolean
                            , subreddit :: String
                            , title :: String
                            , url :: String }

runLinkPost :: LinkPost -> LinkPostRec
runLinkPost (LinkPost p) = p

instance requestableLinkPost :: Requestable LinkPost where
  querystring (LinkPost o) = qsify { resubmit: o.resubmit
                                   , sendreplies: o.sendReplies
                                   , sr: o.subreddit
                                   , title: o.title
                                   , url: o.url
                                   , kind: "link"
                                   , api_type: "json" }

type SelfPostRec = { sendReplies :: Boolean
                   , subreddit :: String
                   , title :: String
                   , body :: String }
newtype SelfPost = SelfPost { sendReplies :: Boolean
                            , subreddit :: String
                            , title :: String
                            , body :: String }

runSelfPost :: SelfPost -> SelfPostRec
runSelfPost (SelfPost p) = p

instance requestableSelfPost :: Requestable SelfPost where
  querystring (SelfPost o) = qsify { sendreplies: o.sendReplies
                                   , sr: o.subreddit
                                   , title: o.title
                                   , text: o.body
                                   , kind: "self"
                                   , api_type: "json" }

type StubbyPostRec = { url :: String, id :: String, name :: String }
newtype StubbyPost = StubbyPost { url :: String
                                , id :: String
                                , name :: String }

runStubbyPost :: StubbyPost -> StubbyPostRec
runStubbyPost (StubbyPost sp) = sp

postToStubbyPost :: Post -> StubbyPost
postToStubbyPost (Post p) =
  StubbyPost { url: p.url
             , id: p.id
             , name: p.name }

commentThreadToStubbyPost :: CommentThread -> StubbyPost
commentThreadToStubbyPost = postToStubbyPost <<< postFromCommentThread

derive instance genericStubbyPost :: Generic StubbyPost

instance showStubbyPost :: Show StubbyPost where
  show = gShow

instance stubbyPostIsForeign :: IsForeign StubbyPost where
  read value = do
    d <- (prop "json" >=> prop "data") value
    url <- readProp "url" d
    id <- readProp "id" d
    name <- readProp "name" d
    return $ StubbyPost { url: url, id: id, name: name }

instance responsableStubbyPost :: Responsable StubbyPost where
  fromForeign = either (Left <<< error <<< show) Right <<< readJSON <<< unsafeToString

type ReplyRec = { body :: String, parent :: String }
newtype Reply = Reply { body :: String, parent :: String }

runReply :: Reply -> ReplyRec
runReply (Reply r) = r

derive instance genericReply :: Generic Reply

instance showReply :: Show Reply where
  show = gShow

instance requestableReply :: Requestable Reply where
  querystring (Reply r) =
    qsify { api_type: "json"
          , text: r.body
          , thing_id: r.parent }

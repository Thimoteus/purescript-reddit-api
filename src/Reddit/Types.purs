module Reddit.Types where

import Prelude

import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Generic (Generic, gShow)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Foreign (readArray, isArray, readString)
import Data.Foreign.Class (readJSON)
import Data.Foreign.Index (prop)
import Data.Traversable (sequence)
import Data.Array (take, filter)
import Data.Array.Unsafe (unsafeIndex)

import Control.Bind ((>=>))

import Node.SimpleRequest (Verb(..))

import Reddit.Util

class Responsable s where
  fromForeign :: Foreign -> s

class Requestable s where
  querystring :: s -> String

instance responsableUnit :: Responsable Unit where
  fromForeign = const unit

type AppInfo = { id :: String
               , secret :: String
               , username :: String
               , password :: String
               , userAgent :: String }

newtype Token = Token { accessToken :: String
                      , tokenType :: String
                      , expiresIn :: Int
                      , scope :: String }

instance responsableToken :: Responsable Token where
  fromForeign = either (const emptyToken) id <<< readJSON <<< unsafeToString

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

type UAToken = Tuple String Token

newtype RedditRequest a = RRequest { endpt :: String
                                   , method :: Verb
                                   , content :: Maybe a }

instance requestableRRequest :: (Requestable a) => Requestable (RedditRequest a) where
  querystring = qsify

instance requestableString :: Requestable String where
  querystring = id

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
runPost (Post o) = o

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
runSubreddit (Subreddit arr) = arr

instance showSubreddit :: Show Subreddit where
  show (Subreddit arr) = "Subreddit " ++ show arr

instance subredditIsForeign :: IsForeign Subreddit where
  read value = do
    children <- value # (prop "data" >=> prop "children" >=> readArray)
    arr <- sequence $ map (readProp "data") children
    return $ Subreddit arr

instance responsableSubreddit :: Responsable Subreddit where
  fromForeign = either (const $ Subreddit []) id <<< readJSON <<< unsafeToString

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

instance commentIsForeign :: IsForeign Comment where
  read value = do
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

data CommentThread = CommentThread Post (Array Comment) | EmptyThread

derive instance genericCommentThread :: Generic CommentThread

instance showCommentThread :: Show CommentThread where
  show x@(CommentThread _ _)= gShow x
  show EmptyThread = "Couldn't find thread."

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
  fromForeign = either (const EmptyThread) id <<< readJSON <<< unsafeToString

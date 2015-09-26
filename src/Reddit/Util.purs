module Reddit.Util where

import Prelude
import Data.Foreign (unsafeFromForeign, Foreign())
import qualified Data.String as S

unsafeToString :: Foreign -> String
unsafeToString = unsafeFromForeign

foreign import qsify :: forall a. a -> String

subbify :: String -> String
subbify s
  | S.take 3 s == "/r/" = s
  | S.take 2 s == "r/" = "/" ++ s
  | otherwise = "/r/" ++ s

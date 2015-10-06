module Reddit.Util where

import Prelude
import Data.Foreign (unsafeFromForeign, Foreign())
import qualified Data.String as S
import Data.Time (Milliseconds(..))
import Unsafe.Coerce (unsafeCoerce)
import Math (floor)
import Data.Maybe.Unsafe (fromJust)
import Data.Int (fromNumber)

unsafeToString :: Foreign -> String
unsafeToString = unsafeFromForeign

unsafeToInt :: Foreign -> Int
unsafeToInt = unsafeFromForeign

foreign import qsify :: forall a. a -> String

subbify :: String -> String
subbify s
  | S.take 3 s == "/r/" = s
  | S.take 2 s == "r/" = "/" ++ s
  | otherwise = "/r/" ++ s

foreign import undefined :: forall a. a

runMilliseconds :: Milliseconds -> Number
runMilliseconds (Milliseconds n) = n

fromFloor :: Number -> Int
fromFloor = fromJust <<< fromNumber <<< floor

closeEnough :: Milliseconds -> Int
closeEnough = fromFloor <<< runMilliseconds

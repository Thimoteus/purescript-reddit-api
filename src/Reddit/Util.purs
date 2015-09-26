module Reddit.Util where

import Data.Foreign (unsafeFromForeign, Foreign())

unsafeToString :: Foreign -> String
unsafeToString = unsafeFromForeign

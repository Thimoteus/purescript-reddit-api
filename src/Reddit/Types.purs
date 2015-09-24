module Reddit.Types where

type AppInfo = { id :: String
               , secret :: String
               , username :: String
               , password :: String
               , userAgent :: String }

foreign import qsify :: forall a. a -> String

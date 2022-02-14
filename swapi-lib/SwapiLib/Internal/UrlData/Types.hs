module SwapiLib.Internal.UrlData.Types where

--------------------------------------------------------------------------------

import Data.Map.Strict (Map)
import Data.Text       (Text)

--------------------------------------------------------------------------------
-- Data types

data UrlData = UrlData
  { udSubdir :: [Text]
    -- ^ The subdirectory of a URL.
    -- e.g "/hey/1" = ["hey", "1"]
  , udParams :: Map Text Text
    -- ^ URL parameters.
    -- e.g "hello/?key1=val1" = fromList [("key1", "val1")]
  }
  deriving stock (Eq, Show)

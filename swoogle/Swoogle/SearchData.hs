module Swoogle.SearchData where

import Data.Text (Text)
import TextShow  qualified as Show (showt)

data SearchData = SearchData
  { sdQuery    :: Text
  , sdPage     :: Word
  -- TOOD: Will change this to an actual resource type
  , sdResource :: Text
  }
  deriving (Eq, Show)


toParamsText :: SearchData -> Text
toParamsText searchData =
  mconcat
    [ "?" <> "query=" <> sdQuery searchData
    , "&page=" <> Show.showt (sdPage searchData)
    , "&resource=" <> sdResource searchData
    ]

module Swoogle.Entry where

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import TextShow qualified as Show (showt)

--------------------------------------------------------------------------------

import Swapi.Resource.Person

--------------------------------------------------------------------------------

data Entry = Entry
  { eTitle :: Text
  , eTags :: [Text]
  , eLink :: Text
  }
  deriving (Eq, Show)

personToEntry :: Person -> Entry
personToEntry p =
  Entry
    { eTitle = coerce @PersonName @Text (pName p)
    , eTags =
        [ heightToText (pHeight p)
        , massToText (pMass p)
        , Text.intercalate " + " (Show.showt <$> pHairColor p) <> " hair"
        , Text.intercalate " + " (Show.showt <$> pEyeColor p) <> " eyes"
        ]
    , eLink = "/people/" <> Show.showt (pId p)
    }

--------------------------------------------------------------------------------

heightToText :: Height -> Text
heightToText height =
  case height of
    Height num -> Show.showt num <> "m"
    UnknownHeight -> "unknown height"

massToText :: Mass -> Text
massToText mass =
  case mass of
    Mass num -> Show.showt num <> " kg"
    UnknownMass -> "unknown mass"

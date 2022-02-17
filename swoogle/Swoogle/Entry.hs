module Swoogle.Entry where

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import TextShow qualified as Show (showt)

--------------------------------------------------------------------------------

import Swapi.Resource.Person
import Swapi.Resource.Film
import Swapi.Resource.Starship
import Swapi.Resource.Vehicle
import Swapi.Resource.Species
import Swapi.Resource.Planet

--------------------------------------------------------------------------------

-- | An entry with a description
data DescriptiveEntry = DescriptiveEntry
  { deTitle :: Text
  , deDescription :: Text
  , deTags :: [Text]
  , deLink :: Text
  }
  deriving stock (Eq, Show)

-- | An entry without a description
data BriefEntry = BriefEntry
  { beTitle :: Text
  , beTags :: [Text]
  , beLink :: Text
  }
  deriving stock (Eq, Show)

-- | `Entry` is used to carry the data fetched from SWAPI, and is then rendered.
data Entry
  = HasDescription DescriptiveEntry
  | NoDescription BriefEntry
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

-- | Parses a `Person` to a more generic `Entry`
fromPerson :: Person -> Entry
fromPerson p =
  NoDescription $ BriefEntry
    { beTitle = coerce @PersonName @Text (pName p)
    , beTags =
        [ heightToText (pHeight p)
        , massToText (pMass p)
        , Text.intercalate " + " (Show.showt <$> pHairColor p) <> " hair"
        , Text.intercalate " + " (Show.showt <$> pEyeColor p) <> " eyes"

        ]
    , beLink = "/people/" <> Show.showt (pId p)
    }

-- | Parses a `Film` to a more generic `Entry`
fromFilm :: Film -> Entry
fromFilm f =
  HasDescription $ DescriptiveEntry
    { deTitle = fTitle f
    , deDescription = coerce @OpeningCrawl @Text (fOpeningCrawl f)
    , deTags =
        [
        ]
    , deLink = "/film/" <> Show.showt (fId f)
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

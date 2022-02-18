module Swoogle.Entry where

--------------------------------------------------------------------------------

import Data.Coerce             (coerce)
import Data.Text               (Text)
import Data.Text               qualified as Text (intercalate)
import TextShow                qualified as Show (showt)

--------------------------------------------------------------------------------

import Swapi.Resource.Person   (Height (Height, UnknownHeight),
                                Mass (Mass, UnknownMass),
                                Person (pEyeColor, pHairColor, pHeight, pId, pMass, pName),
                                PersonName (PersonName))

import Swapi.Resource.Film     (Film (fId, fOpeningCrawl, fTitle),
                                OpeningCrawl (OpeningCrawl))

import Swapi.Resource.Starship (Starship (sId, sName),
                                StarshipName (StarshipName))

import Swapi.Resource.Vehicle  (Vehicle (vId, vName), VehicleName (VehicleName))

import Swapi.Resource.Species  (OriginlessSpecies (hSpId, hSpName),
                                Species (spId, spName),
                                SpeciesName (SpeciesName),
                                SpeciesType (HasOrigin, NoOrigin))

import Swapi.Resource.Planet   (Planet (plId, plName),
                                PlanetName (MkPlanetName))

--------------------------------------------------------------------------------

-- | An entry with a description
data DescriptiveEntry = DescriptiveEntry
  { deTitle       :: Text
  , deDescription :: Text
  , deTags        :: [Text]
  , deLink        :: Text
  }
  deriving stock (Eq, Show)

-- | An entry without a description
data BriefEntry = BriefEntry
  { beTitle :: Text
  , beTags  :: [Text]
  , beLink  :: Text
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

-- | Parses a `Starship` to a more generic `Entry`
fromStarship :: Starship -> Entry
fromStarship s =
  NoDescription $ BriefEntry
    { beTitle = coerce @StarshipName @Text (sName s)
    , beTags =
        [
        ]
    , beLink = "/starship/" <> Show.showt (sId s)
    }

-- | Parses a `Vehicle` to a more generic `Entry`
fromVehicle :: Vehicle -> Entry
fromVehicle v =
  NoDescription $ BriefEntry
    { beTitle = coerce @VehicleName @Text (vName v)
    , beTags =
        [
        ]
    , beLink = "/vehicle/" <> Show.showt (vId v)
    }

-- | Parses a `SpeciesType` to a more generic `Entry`
fromSpecies :: SpeciesType -> Entry
fromSpecies s =
  case s of
    HasOrigin s ->
      NoDescription $ BriefEntry
        { beTitle = coerce @SpeciesName @Text (spName s)
        , beTags =
            [
            ]
        , beLink = "/species/" <> Show.showt (spId s)
        }

    NoOrigin os ->
      NoDescription $ BriefEntry
        { beTitle = coerce @SpeciesName @Text (hSpName os)
        , beTags =
            [
            ]
        , beLink = "/species/" <> Show.showt (hSpId os)
        }

-- | Parses a `Planet` to a more generic `Entry`
fromPlanet :: Planet -> Entry
fromPlanet p =
  NoDescription $ BriefEntry
    { beTitle = coerce @PlanetName @Text (plName p)
    , beTags =
        [
        ]
    , beLink = "/planet/" <> Show.showt (plId p)
    }

--------------------------------------------------------------------------------

heightToText :: Height -> Text
heightToText height =
  case height of
    Height num    -> Show.showt num <> "m"
    UnknownHeight -> "unknown height"

massToText :: Mass -> Text
massToText mass =
  case mass of
    Mass num    -> Show.showt num <> " kg"
    UnknownMass -> "unknown mass"

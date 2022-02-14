module SwapiLib.Internal.Id.Types where

--------------------------------------------------------------------------------
-- External

import Data.Aeson                         (FromJSON (parseJSON),
                                           ToJSON (toJSON), Value (String))
import Data.Aeson                         qualified as Aeson (withText)
import Data.Aeson.Types                   (Parser)
import Data.Kind                          (Type)
import Data.Map.Strict                    qualified as Map
import Data.Text                          (Text)
import TextShow                           (TextShow)
import TextShow                           qualified as Text.Show (showt)

--------------------------------------------------------------------------------
-- Internal

import SwapiLib.Internal.UrlData       qualified as UrlData (getId,
                                                                toUrlText)
import SwapiLib.Internal.UrlData.Types (UrlData (UrlData))

--------------------------------------------------------------------------------
-- Data types

data ResourceId
  = FilmRI FilmId
  | PlanetRI PlanetId
  | SpeciesRI SpeciesId
  | VehicleRI VehicleId
  | StarshipRI StarshipId
  | PersonRI PersonId
  deriving stock (Eq, Show)

newtype FilmId = FilmId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype PlanetId = PlanetId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype SpeciesId = SpeciesId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype VehicleId = VehicleId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype StarshipId = StarshipId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype PersonId = PersonId Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (FilmId :: Type) where
  parseJSON :: Value -> Parser FilmId
  parseJSON =
    Aeson.withText "FilmId" $
      \filmUrl ->
        case UrlData.getId filmUrl of
          Just resourceId -> pure . FilmId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance FromJSON (PlanetId :: Type) where
  parseJSON :: Value -> Parser PlanetId
  parseJSON =
    Aeson.withText "PlanetId" $
      \planetUrl ->
        case UrlData.getId planetUrl of
          Just resourceId -> pure (PlanetId resourceId)
          Nothing         -> fail "Unable to get ID from URL"

instance FromJSON (SpeciesId :: Type) where
  parseJSON :: Value -> Parser SpeciesId
  parseJSON =
    Aeson.withText "SpeciesId" $
      \speciesUrl ->
        case UrlData.getId speciesUrl of
          Just resourceId -> pure . SpeciesId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance FromJSON (VehicleId :: Type) where
  parseJSON :: Value -> Parser VehicleId
  parseJSON =
    Aeson.withText "VehicleId" $
      \vehicleUrl ->
        case UrlData.getId vehicleUrl of
          Just resourceId -> pure . VehicleId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance FromJSON (StarshipId :: Type) where
  parseJSON :: Value -> Parser StarshipId
  parseJSON =
    Aeson.withText "StarshipId" $
      \starshipUrl ->
        case UrlData.getId starshipUrl of
          Just resourceId -> pure . StarshipId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance FromJSON (PersonId :: Type) where
  parseJSON :: Value -> Parser PersonId
  parseJSON =
    Aeson.withText "PersonId" $
      \personUrl ->
        case UrlData.getId personUrl of
          Just resourceId -> pure . PersonId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (FilmId :: Type) where
  toJSON :: FilmId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . FilmRI

instance ToJSON (PlanetId :: Type) where
  toJSON :: PlanetId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . PlanetRI

instance ToJSON (SpeciesId :: Type) where
  toJSON :: SpeciesId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . SpeciesRI

instance ToJSON (VehicleId :: Type) where
  toJSON :: VehicleId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . VehicleRI

instance ToJSON (StarshipId :: Type) where
  toJSON :: StarshipId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . StarshipRI

instance ToJSON (PersonId :: Type) where
  toJSON :: PersonId -> Value
  toJSON = String . UrlData.toUrlText . toUrlData . PersonRI

------------------------------------------------------------------------------
-- Functions

{- |
Encodes a `ResourceId` to a `UrlData`.

__Examples__

@
toUrlData (FilmRI (FilmId 3))
@

Results in:

@
UrlData{udSubdir = ["films", "3"], udParams = Map.fromList []}
@
-}
toUrlData :: ResourceId -> UrlData
toUrlData resourceId =
  let subdir :: [Text]
      subdir =
        case resourceId of
          FilmRI fi     -> ["films", Text.Show.showt fi]
          PlanetRI pli  -> ["planets", Text.Show.showt pli]
          SpeciesRI si  -> ["species", Text.Show.showt si]
          VehicleRI vi  -> ["vehicles", Text.Show.showt vi]
          StarshipRI si -> ["starships", Text.Show.showt si]
          PersonRI pei  -> ["people", Text.Show.showt pei]
  in UrlData subdir Map.empty

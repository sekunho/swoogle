module SwapiClient.Id
  ( FilmId (FilmId)
  , HomeworldId (HomeworldId)
  , SpeciesId (SpeciesId)
  , VehicleId (VehicleId)
  , StarshipId (StarshipId)
  , PersonId (PersonId)
  ) where

--------------------------------------------------------------------------------
-- External

import Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Aeson       qualified as Aeson (withText)
import Data.Aeson.Types (Parser)
import Data.Kind        (Type)
import Data.Text        (Text)
import TextShow         (TextShow)
import TextShow         qualified as Text.Show (showt)

--------------------------------------------------------------------------------
-- Internal

import SwapiClient.Url  (Resource (FilmResource, PeopleResource, PlanetResource, SpeciesResource, StarshipResource, VehicleResource))
import SwapiClient.Url  qualified as Url (getId, resourceUrl)

--------------------------------------------------------------------------------
-- Data types

newtype FilmId = FilmId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype HomeworldId = HomeworldId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype SpeciesId = SpeciesId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype VehicleId = VehicleId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype StarshipId = StarshipId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype PersonId = PersonId Int
  deriving stock (Eq, Show)
  deriving newtype TextShow

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (FilmId :: Type) where
  parseJSON :: Value -> Parser FilmId
  parseJSON =
    Aeson.withText "FilmID" $
      \filmUrl ->
        case Url.getId filmUrl of
          Just resourceId -> pure . FilmId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (FilmId :: Type) where
  toJSON :: FilmId -> Value
  toJSON = String . buildFilmUrl

instance FromJSON (HomeworldId :: Type) where
  parseJSON :: Value -> Parser HomeworldId
  parseJSON val =
    case val of
      String homeworldUrl ->
        case Url.getId homeworldUrl of
          Just resourceId -> pure (HomeworldId resourceId)
          Nothing         -> fail "Unable to get ID from URL"

      _ -> fail "Unexpected type for homeworld URL"

instance ToJSON (HomeworldId :: Type) where
  toJSON :: HomeworldId -> Value
  toJSON = String . buildPlanetUrl

instance FromJSON (SpeciesId :: Type) where
  parseJSON :: Value -> Parser SpeciesId
  parseJSON =
    Aeson.withText "SpeciesId" $
      \speciesUrl ->
        case Url.getId speciesUrl of
          Just resourceId -> pure . SpeciesId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (SpeciesId :: Type) where
  toJSON :: SpeciesId -> Value
  toJSON = String . buildSpeciesUrl

instance FromJSON (VehicleId :: Type) where
  parseJSON :: Value -> Parser VehicleId
  parseJSON =
    Aeson.withText "VehicleId" $
      \vehicleUrl ->
        case Url.getId vehicleUrl of
          Just resourceId -> pure . VehicleId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (VehicleId :: Type) where
  toJSON :: VehicleId -> Value
  toJSON = String . buildVehicleUrl

instance FromJSON (StarshipId :: Type) where
  parseJSON :: Value -> Parser StarshipId
  parseJSON =
    Aeson.withText "StarshipId" $
      \starshipUrl ->
        case Url.getId starshipUrl of
          Just resourceId -> pure . StarshipId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (StarshipId :: Type) where
  toJSON :: StarshipId -> Value
  toJSON = String . buildStarshipUrl

instance FromJSON (PersonId :: Type) where
  -- Gonna parse it from `url` which is why it's from text
  parseJSON :: Value -> Parser PersonId
  parseJSON =
    Aeson.withText "PersonId" $
      \personUrl ->
        case Url.getId personUrl of
          Just resourceId -> pure . PersonId $ resourceId
          Nothing         -> fail "Unable to get ID from URL"

instance ToJSON (PersonId :: Type) where
  toJSON :: PersonId -> Value
  toJSON = String . buildPersonUrl

------------------------------------------------------------------------------
-- Other functions
-- TODO(sekun): Refactor to use `URLData`.
-- TODO(sekun): Refactor remaining URL builders to accommodate the ID sum types

buildFilmUrl :: FilmId -> Text
buildFilmUrl filmId =
  mconcat [Url.resourceUrl FilmResource, Text.Show.showt filmId, "/"]

buildPlanetUrl :: HomeworldId -> Text
buildPlanetUrl homeworldId =
  mconcat
    [ Url.resourceUrl PlanetResource
    , Text.Show.showt homeworldId
    , "/"
    ]

buildSpeciesUrl :: SpeciesId -> Text
buildSpeciesUrl speciesId =
    mconcat
      [ Url.resourceUrl SpeciesResource
      , Text.Show.showt speciesId
      , "/"
      ]

buildVehicleUrl :: VehicleId -> Text
buildVehicleUrl vehicleId =
    mconcat
      [ Url.resourceUrl VehicleResource
      , Text.Show.showt vehicleId
      , "/"
      ]

buildStarshipUrl :: StarshipId -> Text
buildStarshipUrl starshipId =
    mconcat
      [ Url.resourceUrl StarshipResource
      , Text.Show.showt starshipId
      , "/"
      ]

buildPersonUrl :: PersonId -> Text
buildPersonUrl personId =
    mconcat
      [ Url.resourceUrl PeopleResource
      , Text.Show.showt personId
      , "/"
      ]

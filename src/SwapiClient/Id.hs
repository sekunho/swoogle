module SwapiClient.Id
  ( FilmId
  , HomeworldId
  , SpeciesId
  , VehicleId
  , StarshipId
  , PersonId
  , mkFilmId
  , unFilmId
  , mkHomeworldId
  , unHomeworldId
  , mkSpeciesId
  , unSpeciesId
  , mkVehicleId
  , unVehicleId
  , mkStarshipId
  , unStarshipId
  , mkPersonId
  , unPersonId
  ) where

--------------------------------------------------------------------------------
-- External

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (String)
  )
import Data.Aeson qualified as Aeson (withText)
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Text (Text)
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------
-- Internal

import SwapiClient.Url
  ( Resource
      ( Film
      , Planet
      , Species
      , Vehicle
      , Starship, People
      )
  )
import SwapiClient.Url qualified as Url (resourceUrl, getId)

--------------------------------------------------------------------------------
-- Data types

newtype FilmId = FilmId Int
  deriving Show

newtype HomeworldId = HomeworldId Int
  deriving Show

newtype SpeciesId = SpeciesId Int
  deriving Show

newtype VehicleId = VehicleId Int
  deriving Show

newtype StarshipId = StarshipId Int
  deriving Show

newtype PersonId = PersonId Int
  deriving Show


--------------------------------------------------------------------------------
-- Instances

instance FromJSON (FilmId :: Type) where
  parseJSON :: Value -> Parser FilmId
  parseJSON =
    Aeson.withText "FilmID" $
      \filmUrl ->
        case Url.getId filmUrl of
          Right intId ->
            case mkFilmId intId of
              Right filmId -> pure filmId
              Left e -> fail e

          Left e -> fail e

instance ToJSON (FilmId :: Type) where
  toJSON :: FilmId -> Value
  toJSON = String . buildFilmUrl

instance FromJSON (HomeworldId :: Type) where
  parseJSON :: Value -> Parser HomeworldId
  parseJSON =
    Aeson.withText "HomeworldId" $
      \homeworldUrl ->
        case Url.getId homeworldUrl of
          Right intId ->
            case mkHomeworldId intId of
              Right homeworldId -> pure homeworldId
              Left e -> fail e

          Left e -> fail e

instance ToJSON (HomeworldId :: Type) where
  toJSON :: HomeworldId -> Value
  toJSON = String . buildPlanetUrl

instance FromJSON (SpeciesId :: Type) where
  parseJSON :: Value -> Parser SpeciesId
  parseJSON =
    Aeson.withText "SpeciesId" $
      \speciesUrl ->
        case Url.getId speciesUrl of
          Right intId ->
            case mkSpeciesId intId of
              Right speciesId -> pure speciesId
              Left e -> fail e

          Left e -> fail e

instance ToJSON (SpeciesId :: Type) where
  toJSON :: SpeciesId -> Value
  toJSON = String . buildSpeciesUrl

instance FromJSON (VehicleId :: Type) where
  parseJSON :: Value -> Parser VehicleId
  parseJSON =
    Aeson.withText "VehicleId" $
      \vehicleUrl ->
        case Url.getId vehicleUrl of
          Right intId ->
            case mkVehicleId intId of
              Right vehicleId -> pure vehicleId
              Left e -> fail e

          Left e -> fail e

instance ToJSON (VehicleId :: Type) where
  toJSON :: VehicleId -> Value
  toJSON = String . buildVehicleUrl

instance FromJSON (StarshipId :: Type) where
  parseJSON :: Value -> Parser StarshipId
  parseJSON =
    Aeson.withText "StarshipId" $
      \starshipUrl ->
        case Url.getId starshipUrl of
          Right intId ->
            case mkStarshipId intId of
              Right starshipId -> pure starshipId
              Left e -> fail e

          Left e -> fail e

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
          Right intId ->
            case mkPersonId intId of
              Right personId -> pure personId
              Left e -> fail e
          Left e -> fail e

instance ToJSON (PersonId :: Type) where
  toJSON :: PersonId -> Value
  toJSON = String . buildPersonUrl

--------------------------------------------------------------------------------
-- Smart constructors

mkFilmId :: Int -> Either String FilmId
mkFilmId filmId
  | filmId > 0 = Right (FilmId filmId)
  | otherwise = Left "ERROR: ID cannot be negative"

unFilmId :: FilmId -> Int
unFilmId (FilmId filmId) = filmId

mkHomeworldId :: Int -> Either String HomeworldId
mkHomeworldId homeworldId
  | homeworldId > 0 = Right (HomeworldId homeworldId)
  | otherwise = Left "ERROR: ID cannot be negative"

unHomeworldId :: HomeworldId -> Int
unHomeworldId (HomeworldId homeworldId) = homeworldId

mkSpeciesId :: Int -> Either String SpeciesId
mkSpeciesId speciesId
  | speciesId > 0 = Right (SpeciesId speciesId)
  | otherwise = Left "ERROR: ID cannot be negative"

unSpeciesId :: SpeciesId -> Int
unSpeciesId (SpeciesId speciesId) = speciesId

mkVehicleId :: Int -> Either String VehicleId
mkVehicleId vehicleId
  | vehicleId > 0 = Right (VehicleId vehicleId)
  | otherwise = Left "ERROR: ID cannot be negative"

unVehicleId :: VehicleId -> Int
unVehicleId (VehicleId vehicleId) = vehicleId

mkStarshipId :: Int -> Either String StarshipId
mkStarshipId starshipId
  | starshipId > 0 = Right (StarshipId starshipId)
  | otherwise = Left "ERROR: ID cannot be negative"

unStarshipId :: StarshipId -> Int
unStarshipId (StarshipId starshipId) = starshipId

mkPersonId :: Int -> Either String PersonId
mkPersonId pId
  | pId > 0 = Right (PersonId pId)
  | otherwise = Left "ERROR: ID must be greater than 0"

unPersonId :: PersonId -> Int
unPersonId (PersonId pId) = pId

------------------------------------------------------------------------------
-- Other functions
-- TODO(sekun): How do I guarantee that this is a URL? Text can be anything.

buildFilmUrl :: FilmId -> Text
buildFilmUrl filmId =
  mconcat [Url.resourceUrl Film, Text.Show.showt . unFilmId $ filmId, "/"]

buildPlanetUrl :: HomeworldId -> Text
buildPlanetUrl homeworldId =
  mconcat
    [ Url.resourceUrl Planet
    , Text.Show.showt . unHomeworldId $ homeworldId
    , "/"
    ]

buildSpeciesUrl :: SpeciesId -> Text
buildSpeciesUrl speciesId =
  mconcat
    [ Url.resourceUrl Species
    , Text.Show.showt . unSpeciesId $ speciesId
    , "/"
    ]

buildVehicleUrl :: VehicleId -> Text
buildVehicleUrl vehicleId =
  mconcat
    [ Url.resourceUrl Vehicle
    , Text.Show.showt . unVehicleId $ vehicleId
    , "/"
    ]

buildStarshipUrl :: StarshipId -> Text
buildStarshipUrl starshipId =
  mconcat
    [ Url.resourceUrl Starship
    , Text.Show.showt . unStarshipId $ starshipId
    , "/"
    ]

buildPersonUrl :: PersonId -> Text
buildPersonUrl personId =
  mconcat
    [ Url.resourceUrl People
    , Text.Show.showt . unPersonId $ personId
    , "/"
    ]

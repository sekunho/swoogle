{-# LANGUAGE FlexibleInstances #-}

module SwapiClient.Film
  ( Film
      ( fTitle
      , fEpisodeId
      , fOpeningCrawl
      , fDirector
      , fProducers
      , fReleaseDate
      , fCharacters
      , fPlanets
      , fStarships
      , fVehicles
      , fSpecies
      , fCreatedAt
      , fEditedAt
      , fId
      )
  , Director (Director)
  , Producer (Producer)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson
  (FromJSON
  , parseJSON
  , Value
  , (.:)
  )
import Data.Aeson qualified as Aeson (withObject, withText)
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (splitOn)
import Data.Time (Day, UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Id
  ( PersonId
  , HomeworldId
  , VehicleId
  , SpeciesId
  , StarshipId
  , FilmId
  )

--------------------------------------------------------------------------------

data Film = Film
  { fTitle            :: Text
  , fEpisodeId        :: Int
  , fOpeningCrawl     :: Text
  , fDirector         :: Director
  , fProducers        :: [Producer]
  , fReleaseDate      :: Day
  , fCharacters       :: [PersonId]
  , fPlanets          :: [HomeworldId]
  , fStarships        :: [StarshipId]
  , fVehicles         :: [VehicleId]
  , fSpecies          :: [SpeciesId]
  , fCreatedAt        :: UTCTime
  , fEditedAt         :: UTCTime
  , fId               :: FilmId
  } deriving (Eq, Show)

newtype Producer = Producer Text
  deriving (Eq, Show)

newtype Director = Director Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------

instance FromJSON (Director :: Type) where
  parseJSON :: Value -> Parser Director
  parseJSON =
    Aeson.withText "Director" $
      \dirText ->
        pure (Director dirText)

instance FromJSON (Producer :: Type) where
  parseJSON :: Value -> Parser Producer
  parseJSON =
    Aeson.withText "Producer" $
      \prodText ->
        pure (Producer prodText)

instance {-# OVERLAPS #-} FromJSON ([Producer] :: Type) where
  parseJSON :: Value -> Parser [Producer]
  parseJSON =
    Aeson.withText "List of producers" (pure . map Producer . Text.splitOn ", ")

instance FromJSON (Film :: Type) where
  parseJSON :: Value -> Parser Film
  parseJSON =
    Aeson.withObject "Film" $
      \filmObj ->
        Film
          <$> filmObj .: "title"
          <*> filmObj .: "episode_id"
          <*> filmObj .: "opening_crawl"
          <*> filmObj .: "director"
          <*> filmObj .: "producer"
          <*> filmObj .: "release_date"
          <*> filmObj .: "characters"
          <*> filmObj .: "planets"
          <*> filmObj .: "starships"
          <*> filmObj .: "vehicles"
          <*> filmObj .: "species"
          <*> filmObj .: "created"
          <*> filmObj .: "edited"
          <*> filmObj .: "url"

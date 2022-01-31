{-# LANGUAGE FlexibleInstances #-}

module SwapiClient.Film
  ( Film
      ( Film
      , fTitle
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

import Data.Aeson       (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Aeson       qualified as Aeson (object, withObject, withText)
import Data.Aeson.Types (Parser, Value (String))
import Data.Kind        (Type)
import Data.Text        (Text)
import Data.Text        qualified as Text (intercalate, splitOn)
import Data.Time        (Day, UTCTime)
import TextShow         (TextShow)
import TextShow         qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id   (FilmId, HomeworldId, PersonId, SpeciesId, StarshipId,
                         VehicleId)
import SwapiClient.Page (Index (Index, iCount, iNextPage, iPreviousPage, iResults))

--------------------------------------------------------------------------------

data Film = Film
  { fTitle        :: Text
  , fEpisodeId    :: Int
  , fOpeningCrawl :: OpeningCrawl
  , fDirector     :: Director
  , fProducers    :: [Producer]
  , fReleaseDate  :: Day
  , fCharacters   :: [PersonId]
  , fPlanets      :: [HomeworldId]
  , fStarships    :: [StarshipId]
  , fVehicles     :: [VehicleId]
  , fSpecies      :: [SpeciesId]
  , fCreatedAt    :: UTCTime
  , fEditedAt     :: UTCTime
  , fId           :: FilmId
  } deriving (Eq, Show)


-- TODO: Maybe consider producer and director as sum types?
newtype Producer = Producer Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype Director = Director Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype OpeningCrawl = OpeningCrawl Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

--------------------------------------------------------------------------------

instance FromJSON (OpeningCrawl :: Type) where
  parseJSON :: Value -> Parser OpeningCrawl
  parseJSON = Aeson.withText "OpeningCrawl" (pure . OpeningCrawl)

instance ToJSON (OpeningCrawl :: Type) where
  toJSON :: OpeningCrawl -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (Director :: Type) where
  parseJSON :: Value -> Parser Director
  parseJSON = Aeson.withText "Director" (pure . Director)

instance ToJSON (Director :: Type) where
  toJSON :: Director -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (Producer :: Type) where
  parseJSON :: Value -> Parser Producer
  parseJSON = Aeson.withText "Producer" (pure . Producer)

instance ToJSON (Producer :: Type) where
  toJSON :: Producer -> Value
  toJSON = String . Text.Show.showt

instance {-# OVERLAPS #-} FromJSON ([Producer] :: Type) where
  parseJSON :: Value -> Parser [Producer]
  parseJSON =
    Aeson.withText "List of producers" (pure . map Producer . Text.splitOn ", ")

instance {-# OVERLAPS #-} ToJSON ([Producer] :: Type) where
  toJSON :: [Producer] -> Value
  toJSON =
    String . Text.intercalate ", " . map (\(Producer producer) -> producer)

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

instance ToJSON (Film :: Type) where
  toJSON :: Film -> Value
  toJSON film =
    Aeson.object
      [ "title"           .= fTitle film
      , "episode_id"      .= fEpisodeId film
      , "opening_crawl"   .= fOpeningCrawl film
      , "director"        .= fDirector film
      , "producer"        .= fProducers film
      , "release_date"    .= fReleaseDate film
      , "characters"      .= fCharacters film
      , "planets"         .= fPlanets film
      , "starships"       .= fStarships film
      , "vehicles"        .= fVehicles film
      , "species"         .= fSpecies film
      , "created"         .= fCreatedAt film
      , "edited"          .= fEditedAt film
      , "url"             .= fId film
      ]

instance FromJSON (Index Film :: Type) where
  parseJSON :: Value -> Parser (Index Film)
  parseJSON =
    Aeson.withObject "Index" $
      \indexObject ->
        Index
          <$> indexObject .: "count"
          <*> indexObject .: "next"
          <*> indexObject .: "previous"
          <*> indexObject .: "results"

instance ToJSON (Index Film :: Type) where
  toJSON :: Index Film -> Value
  toJSON indexObject =
    Aeson.object
      [ "count"     .= iCount indexObject
      , "next"      .= iNextPage indexObject
      , "previous"  .= iPreviousPage indexObject
      , "results"   .= iResults indexObject
      ]

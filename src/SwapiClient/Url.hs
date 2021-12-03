module SwapiClient.Url
  ( Resource (Root, People, Film, Starship, Vehicle, Species, Planet)
  , resourceUrl
  , getId
  ) where

import Data.Text (Text)
import Data.Text qualified as Text (append, split, {- splitOn -})
import Data.Text.Read qualified as Text.Read (decimal)

--------------------------------------------------------------------------------
-- DATA TYPES

data Resource
  = Root
  | People
  | Film
  | Starship
  | Vehicle
  | Species
  | Planet
  deriving Show

--------------------------------------------------------------------------------
-- FUNCTIONS

baseUrl :: Text
baseUrl = "https://swapi.dev/api"

resourceUrl :: Resource -> Text
resourceUrl resource =
  Text.append baseUrl $
    case resource of
      Root -> "/"
      People -> "/people/"
      Film -> "/films/"
      Starship -> "/starships/"
      Vehicle -> "/vehicles/"
      Species -> "/species/"
      Planet -> "/planets/"

-- Gets the ID of the resource URL
-- > getId ("https://swapi.dev/api/people/1/" :: Text)
-- Right 1
getId :: Text -> Either String Int
getId url =
  case Text.split (== '/') url of
    [_, _, _, _, _, resourceId, _] ->
      case Text.Read.decimal resourceId of
        Right (intId, "") -> Right intId
        Right _ -> Left "ERROR: Unexpected ID format"
        Left e -> Left e

    _ -> Left "ERROR: Unexpected URL format"

-- getPageNumber :: Text -> Either String Int
-- getPageNumber url =
--   case Text.splitOn "?page=" of
--     _ -> _

module SwapiClient.Url
  ( Resource (Root, People, Film, Starship, Vehicle, Specie, Planet)
  , resourceUrl
  ) where

import Data.Text (Text)
import Data.Text qualified as Text (append)

--------------------------------------------------------------------------------
-- DATA TYPES

data Resource
  = Root
  | People
  | Film
  | Starship
  | Vehicle
  | Specie
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
      Specie -> "/species/"
      Planet -> "/planets/"

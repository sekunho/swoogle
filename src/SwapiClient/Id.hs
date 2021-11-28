module SwapiClient.Id
  ( FilmId (FilmId)
  , HomeworldId (HomeworldId)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (String)
  )
import Data.Aeson qualified as Aeson (withText)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (split, append)
import Data.Text.Read qualified as Text.Read (decimal)
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Url (Resource (Film, Planet), resourceUrl)

--------------------------------------------------------------------------------
-- DATA TYPES

newtype FilmId = FilmId Int
  deriving Show

newtype HomeworldId = HomeworldId Int
  deriving Show

--------------------------------------------------------------------------------
-- INSTANCES

instance FromJSON (FilmId :: Type) where
  parseJSON =
    Aeson.withText "FilmID" $
      \filmUrl ->
        case Text.split (== '/') filmUrl of
          [_, _, _, _, _, strId, _] ->
            case Text.Read.decimal strId of
              Right (filmId, "") -> pure . FilmId $ filmId
              Right (_, _) -> fail "ERROR: Unexpected format for film ID value"
              Left e -> fail e

          _ -> fail "ERROR: Invalid URL format"

instance ToJSON (FilmId :: Type) where
  toJSON = String . buildFilmUrl

instance FromJSON (HomeworldId :: Type) where
  parseJSON =
    Aeson.withText "HomeworldId" $
      \homeworldUrl ->
        case Text.split (== '/') homeworldUrl of
          [_, _, _, _, _, strId, _] ->
            case Text.Read.decimal strId of
              Right (homeworldId, "") -> pure . HomeworldId $ homeworldId
              Right (_, _) -> fail "ERROR: Unexpected format for film ID value"
              Left e -> fail e

          _ -> fail "ERROR: Invalid URL format"

instance ToJSON (HomeworldId :: Type) where
  toJSON = String . buildPlanetUrl

--------------------------------------------------------------------------------
-- FUNCTIONS
-- TODO(sekun): How do I guarantee that this is a URL? Text can be anything.

buildFilmUrl :: FilmId -> Text
buildFilmUrl (FilmId filmId) =
  Text.append (resourceUrl Film) . Text.Show.showt $ filmId

buildPlanetUrl :: HomeworldId -> Text
buildPlanetUrl (HomeworldId homeworldId) =
  Text.append (resourceUrl Planet) . Text.Show.showt $ homeworldId

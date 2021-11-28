module SwapiClient.Id
  ( FilmId (FilmId)
  , HomeworldId (HomeworldId)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (Number, String)
  )
import Data.Aeson qualified as Aeson (withText, withScientific)
import Data.Kind (Type)
import Data.Scientific qualified as Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text (split, append)
import Data.Text.Read qualified as Text.Read (decimal)
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Url (Resource (Film), resourceUrl)

--------------------------------------------------------------------------------
-- DATA TYPES

newtype FilmId = FilmId Int
  deriving Show

newtype HomeworldId = HomeworldId Int
  deriving Show

--------------------------------------------------------------------------------
-- INSTANCES
-- TODO(sekun): Parse URL rather than assume it's a number (because it isn't).

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
    Aeson.withScientific "HomeworldId" $
      \homeworldId ->
        case Scientific.toBoundedInteger homeworldId of
          Just intId -> pure $ HomeworldId intId
          Nothing -> fail "ERROR: Homeworld id must be a valid integer"

instance ToJSON (HomeworldId :: Type) where
  toJSON (HomeworldId homeworldId) = Number . fromIntegral $ homeworldId

--------------------------------------------------------------------------------
-- FUNCTIONS

buildFilmUrl :: FilmId -> Text
buildFilmUrl (FilmId filmId) =
  Text.append (resourceUrl Film) . Text.Show.showt $ filmId

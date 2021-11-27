module SwapiClient.Homeworld
  ( HomeworldId (HomeworldId)
  ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number))
import qualified Data.Aeson as Aeson (withScientific)
import Data.Kind (Type)
import qualified Data.Scientific as Scientific (toBoundedInteger)

--------------------------------------------------------------------------------
-- DATA TYPES

newtype HomeworldId = HomeworldId Int
  deriving Show

-- TODO(sekun): Add `Homeworld` record

--------------------------------------------------------------------------------
-- INSTANCES

instance FromJSON (HomeworldId :: Type) where
  parseJSON =
    Aeson.withScientific "HomeworldId" $
      \homeworldId ->
        case Scientific.toBoundedInteger homeworldId of
          Just intId -> pure $ HomeworldId intId
          Nothing -> fail "ERROR: Homeworld id must be a valid integer"

instance ToJSON (HomeworldId :: Type) where
  toJSON (HomeworldId homeworldId) = Number . fromIntegral $ homeworldId

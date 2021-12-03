module SwapiClient.Root
  ( Root (..)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson (withObject, object)
import Data.Aeson.Types
  ( Value
  , Parser
  , FromJSON (parseJSON)
  , ToJSON (toJSON)
  )
import Data.Kind (Type)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Data types

data Root = Root
  { rPeople    :: Text
  , rPlanets   :: Text
  , rFilms     :: Text
  , rSpecies   :: Text
  , rVehicles  :: Text
  , rStarships :: Text
  }
  deriving Show

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (Root :: Type) where
  parseJSON :: Value -> Parser Root
  parseJSON =
    Aeson.withObject "Root" $
      \rootObj ->
        Root
          <$> rootObj .: "people"
          <*> rootObj .: "planets"
          <*> rootObj .: "films"
          <*> rootObj .: "species"
          <*> rootObj .: "vehicles"
          <*> rootObj .: "starships"

instance ToJSON (Root :: Type) where
  toJSON :: Root -> Value
  toJSON root =
    Aeson.object
      [ "people" .= rPeople root
      , "planets" .= rPlanets root
      , "films" .= rFilms root
      , "species" .= rSpecies root
      , "vehicles" .= rVehicles root
      , "starships" .= rStarships root
      ]

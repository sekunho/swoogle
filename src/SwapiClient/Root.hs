module SwapiClient.Root
  ( Root
    ( rFilms
    , rPeople
    , rPlanets
    , rSpecies
    , rStarships
    , rVehicles
    )
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
  { rFilms     :: Text
  , rPeople    :: Text
  , rPlanets   :: Text
  , rSpecies   :: Text
  , rStarships :: Text
  , rVehicles  :: Text
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (Root :: Type) where
  parseJSON :: Value -> Parser Root
  parseJSON =
    Aeson.withObject "Root" $
      \rootObj ->
        Root
          <$> rootObj .: "films"
          <*> rootObj .: "people"
          <*> rootObj .: "planets"
          <*> rootObj .: "species"
          <*> rootObj .: "starships"
          <*> rootObj .: "vehicles"

instance ToJSON (Root :: Type) where
  toJSON :: Root -> Value
  toJSON root =
    Aeson.object
      [ "films"     .= rFilms root
      , "people"    .= rPeople root
      , "planets"   .= rPlanets root
      , "species"   .= rSpecies root
      , "starships" .= rStarships root
      , "vehicles"  .= rVehicles root
      ]

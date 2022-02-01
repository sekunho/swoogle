{- |
This module doesn't have much of a purpose.

The `Root` is supposed to give an overview on what resources are available, and
the URL for each resource. Initially, I wanted to work on it like the others,
but this doesn't seem all that useful. I only used it to practice using @aeson@,
but after that, there's not much else to this.
-}
module SwapiClient.Resource.Root
  ( Root
    ( Root
    , rFilms
    , rPeople
    , rPlanets
    , rSpecies
    , rStarships
    , rVehicles
    )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson       ((.:), (.=))
import Data.Aeson       qualified as Aeson (object, withObject)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON), Value)
import Data.Kind        (Type)
import Data.Text        (Text)

--------------------------------------------------------------------------------
-- Data types

-- | Represents the root resource
data Root = Root
  { rFilms     :: Text -- ^ URL for the films resource
  , rPeople    :: Text -- ^ URL for the people resource
  , rPlanets   :: Text -- ^ URL for the planets resource
  , rSpecies   :: Text -- ^ URL for the species resource
  , rStarships :: Text -- ^ URL for the starhips resource
  , rVehicles  :: Text -- ^ URL for the vehicles resource
  }
  deriving stock
  ( Eq   -- ^ Compare `Root`s with each other
  , Show -- ^ Encode `Height` as `String` through `show`
  )

--------------------------------------------------------------------------------
-- Instances

-- | Decode a JSON object to `Root`.
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

-- | Encode a `Root` to a JSON object.
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

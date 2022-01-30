module SwapiClient.Vehicle
  ( VehicleName (VehicleName)
  , VehicleModel (VehicleModel)
  , VehicleLength (VehicleLength)
  , VehicleClass
    ( VCWheeled
    , VCRepulsorcraft
    , VCStarfighter
    , VCAirspeeder
    , VCSpaceBomber
    , VCAssaultWalker
    , VCWalker
    , VCSailBarge
    , VCRepulsorcraftCargoSkiff
    , VCSpeeder
    , VCLandingCraft
    , VCSubmarine
    , VCGunship
    , VCTransport
    )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (parseJSON, (.:))
import Data.Aeson.Types (FromJSON, ToJSON, Parser, Value)
import Data.Aeson qualified as Aeson (withText, withObject)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (toLower)
import Data.Text.Read qualified as Text.Read (double)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Id (PersonId, FilmId, VehicleId)
import SwapiClient.Starship
  ( CargoCapacity
  , Consumable
  , Cost
  , Manufacturer
  , MaxAtmospheringSpeed
  , PassengerLimit
  , RequiredCrew
  , Wrapped (Wrapped)
  )

--------------------------------------------------------------------------------
-- Data types

newtype VehicleName = VehicleName Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

newtype VehicleModel = VehicleModel Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

newtype VehicleLength = VehicleLength Double
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Double)

data VehicleClass
  = VCWheeled
  | VCRepulsorcraft
  | VCStarfighter
  | VCAirspeeder
  | VCSpaceBomber
  | VCAssaultWalker
  | VCWalker
  | VCSailBarge
  | VCRepulsorcraftCargoSkiff
  | VCSpeeder
  | VCLandingCraft
  | VCSubmarine
  | VCGunship
  | VCTransport
  deriving stock (Eq, Show)

data Vehicle = Vehicle
  { vName :: VehicleName
  , vModel :: VehicleModel
  , vManufacturer :: Manufacturer
  , vCost :: Cost
  , vLength :: VehicleLength
  , vMaxAtmospheringSpeed :: MaxAtmospheringSpeed
  , vRequiredCrew :: RequiredCrew
  , vPassengerLimit :: PassengerLimit
  , vCargoCapacity :: CargoCapacity
  , vConsumables :: Consumable
  , vVehicleClass :: VehicleClass
  , vPilots :: [PersonId]
  , vFilms :: [FilmId]
  , vCreatedAt :: UTCTime
  , vEditedAt :: UTCTime
  , vId :: VehicleId
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (VehicleName :: Type) where
  parseJSON = Aeson.withText "VehicleName" (pure . VehicleName)

instance FromJSON (VehicleModel :: Type) where
  parseJSON = Aeson.withText "VehicleModel" (pure . VehicleModel)

instance FromJSON (VehicleLength :: Type) where
  parseJSON = Aeson.withText "VehicleLength" $
    \val ->
      case Text.Read.double val of
        Right (vehicleLength, "") -> pure $ VehicleLength vehicleLength
        Right _ -> fail "VehicleLength is just supposed to be a number"
        Left s -> fail s

instance FromJSON (VehicleClass :: Type) where
  parseJSON =
    Aeson.withText "VehicleClass" $
      \val ->
        case Text.toLower val of
          "wheeled" -> pure VCWheeled
          "repulsorcraft" -> pure VCRepulsorcraft
          "starfighter" -> pure VCStarfighter
          "airspeeder" -> pure VCAirspeeder
          "space bomber" -> pure VCSpaceBomber
          "assault walker" -> pure VCAssaultWalker
          "walker" -> pure VCWalker
          "sail barge" -> pure VCSailBarge
          "repulsorcraft cargo skiff" -> pure VCRepulsorcraftCargoSkiff
          "speeder" -> pure VCSpeeder
          "landing craft" -> pure VCLandingCraft
          "submarine" -> pure VCSubmarine
          "gunship" -> pure VCGunship
          "transport" -> pure VCTransport
          _ -> fail "Unexpected value for VehicleClass"

instance FromJSON (Vehicle :: Type) where
  parseJSON :: Value -> Parser Vehicle
  parseJSON =
    Aeson.withObject "Vehicle" $
      \val ->
        Vehicle
          <$> val .: "name"
          <*> val .: "model"
          <*> val .: "manufacturer"
          <*> val .: "cost_in_credits"
          <*> val .: "length"
          <*> val .: "max_atmosphering_speed"
          <*> val .: "crew"
          <*> val .: "passengers"
          <*> val .: "cargo_capacity"
          <*> val .: "consumables"
          <*> val .: "vehicle_class"
          <*> val .: "pilots"
          <*> val .: "films"
          <*> val .: "created"
          <*> val .: "edited"
          <*> val .: "url"

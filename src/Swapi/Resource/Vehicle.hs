module Swapi.Resource.Vehicle
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
    , VCWheeledWalker
    , VCFireSuppressionShip
    , VCDroidStarfighter
    , VCDroidTank
    )
  , Vehicle
    ( vName
    , vModel
    , vManufacturer
    , vCost
    , vLength
    , vMaxAtmospheringSpeed
    , vRequiredCrew
    , vPassengerLimit
    , vCargoCapacity
    , vConsumables
    , vVehicleClass
    , vPilots
    , vFilms
    , vCreatedAt
    , vEditedAt
    , vId
    )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson              (parseJSON, (.:))
import Data.Aeson              qualified as Aeson (withObject, withText)
import Data.Aeson.Types        (FromJSON, Parser, ToJSON, Value)
import Data.Kind               (Type)
import Data.Text               (Text)
import Data.Text               qualified as Text (stripEnd, toLower)
import Data.Text.Read          qualified as Text.Read (double)
import Data.Time               (UTCTime)

--------------------------------------------------------------------------------

import Swapi.Id                (FilmId, PersonId, VehicleId)
import Swapi.Internal.Page     (Index (Index))
import Swapi.Resource.Starship (CargoCapacity, Consumable, Cost, Manufacturer,
                                MaxAtmospheringSpeed, PassengerLimit,
                                RequiredCrew, Wrapped (Wrapped))

--------------------------------------------------------------------------------
-- Data types

newtype VehicleName = VehicleName Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

newtype VehicleModel = VehicleModel Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

data VehicleLength
  = VehicleLength Double
  | UnknownVehicleLength
  deriving stock (Eq, Show)

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
  | VCWheeledWalker
  | VCFireSuppressionShip
  | VCDroidStarfighter
  | VCDroidTank
  deriving stock (Eq, Show)

data Vehicle = Vehicle
  { vName                 :: VehicleName
  , vModel                :: VehicleModel
  , vManufacturer         :: Manufacturer
  , vCost                 :: Cost
  , vLength               :: VehicleLength
  , vMaxAtmospheringSpeed :: MaxAtmospheringSpeed
  , vRequiredCrew         :: RequiredCrew
  , vPassengerLimit       :: PassengerLimit
  , vCargoCapacity        :: CargoCapacity
  , vConsumables          :: Consumable
  , vVehicleClass         :: VehicleClass
  , vPilots               :: [PersonId]
  , vFilms                :: [FilmId]
  , vCreatedAt            :: UTCTime
  , vEditedAt             :: UTCTime
  , vId                   :: VehicleId
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
    \case
      "unknown" -> pure UnknownVehicleLength
      val ->
        case Text.Read.double . Text.stripEnd $ val of
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
          "air speeder" -> pure VCAirspeeder
          "airspeeder" -> pure VCAirspeeder
          "space/planetary bomber" -> pure VCSpaceBomber
          "assault walker" -> pure VCAssaultWalker
          "walker" -> pure VCWalker
          "droid tank" -> pure VCDroidTank
          "sail barge" -> pure VCSailBarge
          "repulsorcraft cargo skiff" -> pure VCRepulsorcraftCargoSkiff
          "speeder" -> pure VCSpeeder
          "landing craft" -> pure VCLandingCraft
          "submarine" -> pure VCSubmarine
          "gunship" -> pure VCGunship
          "transport" -> pure VCTransport
          "wheeled walker" -> pure VCWheeledWalker
          "fire suppression ship" -> pure VCFireSuppressionShip
          "droid starfighter" -> pure VCDroidStarfighter
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

instance FromJSON ((Index Vehicle) :: Type) where
  parseJSON :: Value -> Parser (Index Vehicle)
  parseJSON =
    Aeson.withObject "Index Vehicle" $
      \val ->
        Index
          <$> val .: "count"
          <*> val .: "next"
          <*> val .: "previous"
          <*> val .: "results"

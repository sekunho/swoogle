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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time

--------------------------------------------------------------------------------

import SwapiClient.Id (PersonId, FilmId, VehicleId)
import SwapiClient.Starship

--------------------------------------------------------------------------------
-- Data types

newtype VehicleName = VehicleName Text
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via StarshipName

newtype VehicleModel = VehicleModel Text
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via StarshipName

newtype VehicleLength = VehicleLength Double
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via StarshipLength

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


{-# language DerivingVia #-}

module SwapiClient.Vehicle where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import TextShow (TextShow)

import SwapiClient.Id (PersonId, FilmId, VehicleId)
import SwapiClient.Starship
  ( Cost
  , RequiredCrew
  , PassengerLimit
  , CargoCapacity
  , Consumable
  , MaxAtmospheringSpeed
  , Manufacturer
  , StarshipName (StarshipName)
  )

--------------------------------------------------------------------------------

newtype VehicleName = VehicleName Text
  deriving stock (Eq, Show)
  deriving newtype TextShow
  deriving (FromJSON, ToJSON) via StarshipName

newtype VehicleModel = VehicleModel Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype VehicleLength = VehicleLength Double
  deriving stock (Eq, Show)
  deriving newtype TextShow

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

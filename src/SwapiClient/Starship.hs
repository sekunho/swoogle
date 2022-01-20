module SwapiClient.Starship
  ( Starship
    ( sName
    , sModel
    , sManufacturer
    , sCostInCredits
    , sLength
    , sMaxAtmosphericSpeed
    , sRequiredCrew
    , sPassengerLimit
    , sCargoCapacity
    , sConsumables
    , sHyperdriveRating
    , sMaxMegalight
    , sStarshipClass
    , sPilots
    , sFilms
    , sCreatedAt
    , sEditedAt
    , sId
    )
  , StarshipClass
    ( Corvette
    , StarDestroyer
    , LandingCraft
    , DeepSpaceMobileBattlestation
    , LightFreighter
    , AssaultStarfighter
    , Starfighter
    , StarDreadnought
    , MediumTransport
    )
  , MaxMegalight (MaxMegalight)
  , HyperdriveRating (HyperdriveRating)
  , CargoCapacity (CargoCapacity)
  , StarshipLength (StarshipLength)
  , PassengerLimit (PassengerLimit, PLNotApplicable)
  , MaxAtmosphericSpeed (MaxSpeed, MASNotApplicable)
  , Credits (Credits)
  , Manufacturer (Manufacturer)
  , StarshipModel (StarshipModel)
  , StarshipName (StarshipName)
  , Consumable (Week, Month, Year)
  , RequiredCrew (CrewRange, CrewNumber)
  ) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Person (Person)
import SwapiClient.Film (Film)
import SwapiClient.Id (StarshipId)

--------------------------------------------------------------------------------

data RequiredCrew
  = CrewRange Word Word
  | CrewNumber Word
  deriving (Eq, Show)

data Consumable
  = Week Word
  | Month Word
  | Year Word
  deriving (Eq, Show)

newtype StarshipName = StarshipName Text
  deriving (Eq, Show)

newtype StarshipModel = StarshipModel Text
  deriving (Eq, Show)

newtype Manufacturer = Manufacturer Text
  deriving (Eq, Show)

newtype Credits = Credits Word
  deriving (Eq, Show)

data MaxAtmosphericSpeed
  = MaxSpeed Word
  | MASNotApplicable
  deriving (Eq, Show)

data PassengerLimit
  = PassengerLimit Word
  | PLNotApplicable
  deriving (Eq, Show)

newtype StarshipLength = StarshipLength Word
  deriving (Eq, Show)

newtype CargoCapacity = CargoCapacity Word
  deriving (Eq, Show)

newtype HyperdriveRating = HyperdriveRating Double
  deriving (Eq, Show)

newtype MaxMegalight = MaxMegalight Word
  deriving (Eq, Show)

data StarshipClass
  = Corvette
  | StarDestroyer
  | LandingCraft
  | DeepSpaceMobileBattlestation
  | LightFreighter
  | AssaultStarfighter
  | Starfighter
  | StarDreadnought
  | MediumTransport
  deriving (Eq, Show)

data Starship = Starship
  { sName :: StarshipName
  , sModel :: StarshipModel
  , sManufacturer :: Manufacturer
  , sCostInCredits :: Credits
  , sLength :: StarshipLength
  , sMaxAtmosphericSpeed :: MaxAtmosphericSpeed
  , sRequiredCrew :: RequiredCrew
  , sPassengerLimit :: PassengerLimit
  , sCargoCapacity :: CargoCapacity
  , sConsumables :: Consumable
  , sHyperdriveRating :: HyperdriveRating
  , sMaxMegalight :: MaxMegalight
  , sStarshipClass :: StarshipClass
  , sPilots :: [Person]
  , sFilms :: [Film]
  , sCreatedAt :: UTCTime
  , sEditedAt :: UTCTime
  , sId :: StarshipId
  }

--------------------------------------------------------------------------------

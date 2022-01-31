module SwapiClient.Planet (
  ) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Time (UTCTime)
import TextShow  (TextShow)

--------------------------------------------------------------------------------

import SwapiClient.Id (PersonId, FilmId, HomeworldId)

--------------------------------------------------------------------------------
-- Data types

newtype PlanetName = MkPlanetName Text
  deriving stock (Eq, Show)
  deriving newtype (TextShow)

newtype RotationPeriod = MkRotationPeriod Word
  deriving stock (Eq, Show)
  deriving newtype (TextShow)

newtype OrbitalPeriod = MkOrbitalPeriod Word
  deriving stock (Eq, Show)
  deriving newtype (TextShow)

newtype Diameter = MkDiameter Word
  deriving stock (Eq, Show)
  deriving newtype (TextShow)

data Climate
  = Arctic
  | Arid
  | ArtificialTemperate
  | Frigid
  | Frozen
  | Hot
  | Humid
  | Moist
  | Murky
  | Polluted
  | Rocky
  | Temperate
  | Tropical
  | UnknownClimate
  | Windy
  deriving stock (Eq, Show)

data Gravity
  = Standard Word
  | UnknownGravity
  | GravityNotApplicable
  deriving stock (Eq, Show)

data Terrain
  = Desert
  | Grassland
  | Mountain
  | Jungle
  | Rainforest
  | IceCave
  | MountainRange
  | GasGiant
  | Ocean
  | Lake
  | GrassyHill
  | Swamp
  | Forest
  | Rock
  | Barren
  | Scrubland
  | Savanna
  | Canyon
  | Sinkhole
  | Volcanoe
  | LavaRiver
  | Cave
  | Rivers
  | AirlessAsteroid
  | Glacier
  | IceCanyon
  | FungusForest
  | Field
  | RockArch
  | Cityscape
  | Plain
  | Urban
  | Hill
  | Bog
  | RockyIsland
  | Mesas
  | UnknownTerrain
  | Reef
  | Island
  | RockyDesert
  | Valley
  | Tundra
  | Ash
  | ToxicCloudsea
  | Plateau
  | Verdant
  | AcidPool
  | RockyCanyon

data SurfaceWaterPercent
  = Percentage Double
  | UnknownPercentage
  deriving (Eq, Show)

data Population
  = Population Word
  | UnknownPopulation
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

data Planet = Planet
  { plName :: PlanetName
  , plRotationPeriod :: RotationPeriod
  , plOrbitalPeriod :: OrbitalPeriod
  , plDiameter :: Diameter
  , plClimate :: Climate
  , plGravity :: Gravity
  , plTerrain :: Terrain
  , plSurfaceWaterPercent :: SurfaceWaterPercent
  , plPopulation :: Population
  , plResidents :: [PersonId]
  , plFilms :: [FilmId]
  , plCreatedAt :: UTCTime
  , plEditedAt :: UTCTime
  , plId :: HomeworldId
  }

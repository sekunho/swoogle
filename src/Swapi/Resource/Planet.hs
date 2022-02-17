module Swapi.Resource.Planet
  ( PlanetName (MkPlanetName)
  , RotationPeriod (RotationPeriod, UnknownRotationPeriod)
  , OrbitalPeriod (OrbitalPeriod, UnknownOrbitalPeriod)
  , Diameter (Diameter, UnknownDiameter)
  , Climate
    ( Arctic
    , Arid
    , ArtificialTemperate
    , Frigid
    , Frozen
    , Hot
    , Humid
    , Moist
    , Murky
    , Polluted
    , RockyClimate
    , Subarctic
    , Superheated
    , Temperate
    , Tropical
    , UnknownClimate
    , Windy
    )
  , Gravity
    ( Standard
    , UnknownGravity
    , GravityNotApplicable
    )
  , Terrain
    ( Desert
    , Grass
    , Grassland
    , Mountain
    , Jungle
    , Rainforest
    , IceCave
    , MountainRange
    , GasGiant
    , Ocean
    , Lake
    , GrassyHill
    , Swamp
    , Forest
    , Rock
    , RockyTerrain
    , Barren
    , Scrubland
    , Savannah
    , Canyon
    , Cliff
    , Sinkhole
    , Volcanoe
    , LavaRiver
    , Cave
    , Cities
    , River
    , AirlessAsteroid
    , Glacier
    , IceCanyon
    , FungusForest
    , Field
    , RockArch
    , Cityscape
    , Plain
    , Urban
    , Hill
    , Bog
    , RockyIsland
    , Mesas
    , UnknownTerrain
    , Reef
    , Island
    , RockyDesert
    , Valley
    , Vines
    , Tundra
    , Ash
    , ToxicCloudsea
    , Sea
    , Plateau
    , Verdant
    , AcidPool
    , RockyCanyon
    )
  , Population (Population, UnknownPopulation)
  , Planet
    ( plName
    , plRotationPeriod
    , plOrbitalPeriod
    , plDiameter
    , plClimate
    , plGravity
    , plTerrain
    , plSurfaceWaterPercent
    , plPopulation
    , plResidents
    , plFilms
    , plCreatedAt
    , plEditedAt
    , plId
    )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson                (parseJSON, (.:))
import Data.Aeson                qualified as Aeson (withObject, withText)
import Data.Aeson.Types          (FromJSON, Parser, Value (String))
import Data.Kind                 (Type)
import Data.Text                 (Text)
import Data.Text                 qualified as Text (split, splitOn, strip,
                                                    toLower, unpack)
import Data.Text.Read            qualified as Text.Read (decimal, double)
import Data.Time                 (UTCTime)
import TextShow                  (TextShow)

--------------------------------------------------------------------------------

import Swapi.Id            (FilmId, PersonId, PlanetId)
import Swapi.Internal.Page (Index (Index))

--------------------------------------------------------------------------------
-- Data types

type PlanetName :: Type
newtype PlanetName = MkPlanetName Text
  deriving stock (Eq, Show)
  deriving newtype (TextShow)

type RotationPeriod :: Type
data RotationPeriod
  = RotationPeriod Word
  | UnknownRotationPeriod
  deriving stock (Eq, Show)

type OrbitalPeriod :: Type
data OrbitalPeriod
  = OrbitalPeriod Word
  | UnknownOrbitalPeriod
  deriving stock (Eq, Show)

type Diameter :: Type
data Diameter
  = Diameter Word
  | UnknownDiameter
  deriving stock (Eq, Show)

type Climate :: Type
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
  | RockyClimate  -- I honestly think this is a mistake on swapi's end.
  | Subarctic
  | Superheated
  | Temperate
  | Tropical
  | UnknownClimate
  | Windy
  deriving stock (Eq, Show)

type Gravity :: Type
data Gravity
  = Standard Double
  | UnknownGravity
  | GravityNotApplicable
  deriving stock (Eq, Show)

type Terrain :: Type
data Terrain
  = Desert
  | Grassland
  | Grass
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
  | RockyTerrain
  | Barren
  | Scrubland
  | Savannah
  | Canyon
  | Cliff
  | Sinkhole
  | Volcanoe
  | Vines
  | LavaRiver
  | Cities
  | Cave
  | River
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
  | Sea
  | Plateau
  | Verdant
  | AcidPool
  | RockyCanyon
  deriving stock (Eq, Show)

type SurfaceWaterPercent :: Type
data SurfaceWaterPercent
  = Percentage Double
  | UnknownPercentage
  deriving stock (Eq, Show)

type Population :: Type
data Population
  = Population Word
  | UnknownPopulation
  deriving stock (Eq, Show)

type Planet :: Type
data Planet = Planet
  { plName                :: PlanetName
  , plRotationPeriod      :: RotationPeriod
  , plOrbitalPeriod       :: OrbitalPeriod
  , plDiameter            :: Diameter
  , plClimate             :: [Climate]
  , plGravity             :: Gravity
  , plTerrain             :: [Terrain]
  , plSurfaceWaterPercent :: SurfaceWaterPercent
  , plPopulation          :: Population
  , plResidents           :: [PersonId]
  , plFilms               :: [FilmId]
  , plCreatedAt           :: UTCTime
  , plEditedAt            :: UTCTime
  , plId                  :: PlanetId
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (PlanetName :: Type) where
  parseJSON :: Value -> Parser PlanetName
  parseJSON = Aeson.withText "PlanetName" (pure . MkPlanetName)

instance FromJSON (RotationPeriod :: Type) where
  parseJSON :: Value -> Parser RotationPeriod
  parseJSON =
    Aeson.withText "RotationPeriod" $
      \case
        "unknown" -> pure UnknownRotationPeriod
        val ->
          case Text.Read.decimal val of
            Right (rotationPeriod, "") -> pure (RotationPeriod rotationPeriod)
            Right _ -> fail "Rotation period should be unknown or a number string"
            Left e ->
              fail ("Unexpected value for rotation period: " <> e)

instance FromJSON (OrbitalPeriod :: Type) where
  parseJSON :: Value -> Parser OrbitalPeriod
  parseJSON =
    Aeson.withText "OrbitalPeriod" $
      \case
        "unknown" -> pure UnknownOrbitalPeriod
        val ->
          case Text.Read.decimal val of
            Right (rotationPeriod, "") -> pure (OrbitalPeriod rotationPeriod)
            Right _ -> fail "Orbital period should be unknown or a number string"
            Left e ->
              fail ("Unexpected value for orbital period: " <> e)

instance FromJSON (Diameter :: Type) where
  parseJSON :: Value -> Parser Diameter
  parseJSON =
    Aeson.withText "Diameter" $
      \case
        "unknown" -> pure UnknownDiameter
        val ->
          case Text.Read.decimal val of
            Right (diameter, "") -> pure (Diameter diameter)
            Right _ -> fail "Diameter should be unknown or a number string"
            Left e -> fail e

instance FromJSON (Climate :: Type) where
  parseJSON :: Value -> Parser Climate
  parseJSON =
    Aeson.withText "Climate" $
      \val ->
        case Text.toLower (Text.strip val) of
          -- FIXME: Yes, swapi mispelled it. Don't crucify anyone.
          "artic" -> pure Arctic
          "arctic" -> pure Arctic
          "arid" -> pure Arid
          "artificial temperate" -> pure ArtificialTemperate
          "frigid" -> pure Frigid
          "frozen" -> pure Frozen
          "hot" -> pure Hot
          "humid" -> pure Humid
          "moist" -> pure Moist
          "murky" -> pure Murky
          "polluted" -> pure Polluted
          "rocky" -> pure RockyClimate
          "subartic" -> pure Subarctic
          "subarctic" -> pure Subarctic
          "superheated" -> pure Superheated
          "temperate" -> pure Temperate
          "tropical" -> pure Tropical
          "unknown" -> pure UnknownClimate
          "windy" -> pure Windy
          c -> fail ("Unexpected value for climate: " <> Text.unpack c)

instance {-# OVERLAPS #-} FromJSON ([Climate] :: Type) where
  parseJSON :: Value -> Parser [Climate]
  parseJSON =
    Aeson.withText "[Climate]" $
        mapM (parseJSON @Climate . String) . Text.splitOn ", "


instance FromJSON (Gravity :: Type) where
  parseJSON :: Value -> Parser Gravity
  parseJSON =
    Aeson.withText "Gravity" $
      \val ->
        case Text.toLower val of
          "unknown" -> pure UnknownGravity
          "n/a" -> pure GravityNotApplicable
          val' ->
            case Text.splitOn ", " val' of
              {- TODO: Yes, currently the API can have multiple gravity entries
                 decoded as a string. I can't find a way to deal with it, so I'm
                 just using the first gravity value. It only happens once, though.
              -}
              (gravity:_) ->
                case decodeGravity gravity of
                  Right g -> pure g
                  Left e  -> fail e

              [] -> fail "Unexpected value for gravity"
    where
      decodeGravity :: Text -> Either String Gravity
      decodeGravity grav =
        case Text.split (== ' ') (Text.toLower grav) of
          (grav':_) ->
            case Text.Read.double grav' of
              Right (gravity, "") -> Right (Standard gravity)
              Right (gravity, " standard") -> Right (Standard gravity)
              Right e -> Left ("Unexpected value for gravity: " <> show e)
              Left e -> Left e

          [] -> Left ("Unexpected format for gravity: " <> Text.unpack grav)

instance FromJSON (Terrain :: Type) where
  parseJSON :: Value -> Parser Terrain
  parseJSON =
    Aeson.withText "Terrain" $
      \val ->
        -- Most have plural versions since the API isn't so consistent.
        case Text.toLower (Text.strip val) of
          "desert"            -> pure Desert
          "deserts"           -> pure Desert
          "grass"             -> pure Grass
          "grassland"         -> pure Grassland
          "grasslands"        -> pure Grassland
          "mountain"          -> pure Mountain
          "mountains"         -> pure Mountain
          "jungle"            -> pure Jungle
          "jungles"           -> pure Jungle
          "rainforest"        -> pure Rainforest
          "rainforests"       -> pure Rainforest
          "ice cave"          -> pure IceCave
          "ice caves"         -> pure IceCave
          "mountain range"    -> pure MountainRange
          "mountain ranges"   -> pure MountainRange
          "gas giant"         -> pure GasGiant
          "gas giants"        -> pure GasGiant
          "ocean"             -> pure Ocean
          "oceans"            -> pure Ocean
          "lake"              -> pure Lake
          "lakes"             -> pure Lake
          "grassy hill"       -> pure GrassyHill
          "grassy hills"      -> pure GrassyHill
          "swamp"             -> pure Swamp
          "swamps"            -> pure Swamp
          "forest"            -> pure Forest
          "forests"           -> pure Forest
          "rock"              -> pure Rock
          "rocky"             -> pure RockyTerrain
          "barren"            -> pure Barren
          "scrubland"         -> pure Scrubland
          "scrublands"        -> pure Scrubland
          "savanna"           -> pure Savannah
          "savannas"          -> pure Savannah
          "savannah"          -> pure Savannah
          "savannahs"         -> pure Savannah
          "canyon"            -> pure Canyon
          "canyons"           -> pure Canyon
          "sinkhole"          -> pure Sinkhole
          "sinkholes"         -> pure Sinkhole
          "volcanoe"          -> pure Volcanoe
          "volcanoes"         -> pure Volcanoe
          "vines"             -> pure Vines
          "lava river"        -> pure LavaRiver
          "lava rivers"       -> pure LavaRiver
          "cave"              -> pure Cave
          "cities"            -> pure Cities
          "caves"             -> pure Cave
          "river"             -> pure River
          "rivers"            -> pure River
          "airless asteroid"  -> pure AirlessAsteroid
          "airless asteroids" -> pure AirlessAsteroid
          "glacier"           -> pure Glacier
          "glaciers"          -> pure Glacier
          "ice canyon"        -> pure IceCanyon
          "ice canyons"       -> pure IceCanyon
          "fungus forest"     -> pure FungusForest
          "fungus forests"    -> pure FungusForest
          "field"             -> pure Field
          "fields"            -> pure Field
          "rock arch"         -> pure RockArch
          "rock arches"       -> pure RockArch
          "cityscape"         -> pure Cityscape
          "cityscapes"        -> pure Cityscape
          "plain"             -> pure Plain
          "plains"            -> pure Plain
          "urban"             -> pure Urban
          "hill"              -> pure Hill
          "hills"             -> pure Hill
          "bog"               -> pure Bog
          "bogs"              -> pure Bog
          "rocky island"      -> pure RockyIsland
          "rocky islands"     -> pure RockyIsland
          "mesas"             -> pure Mesas
          "unknown"           -> pure UnknownTerrain
          "reef"              -> pure Reef
          "reefs"             -> pure Reef
          "island"            -> pure Island
          "islands"           -> pure Island
          "rocky desert"      -> pure RockyDesert
          "rocky deserts"     -> pure RockyDesert
          "valley"            -> pure Valley
          "valleys"           -> pure Valley
          "tundra"            -> pure Tundra
          "tundras"           -> pure Tundra
          "ash"               -> pure Ash
          "ashes"             -> pure Ash
          "toxic cloudsea"    -> pure ToxicCloudsea
          "toxic cloudseas"   -> pure ToxicCloudsea
          "sea"               -> pure Sea
          "seas"              -> pure Sea
          "plateau"           -> pure Plateau
          "plateaus"          -> pure Plateau
          "verdant"           -> pure Verdant
          "verdants"          -> pure Verdant
          "acid pool"         -> pure AcidPool
          "acid pools"        -> pure AcidPool
          "rocky canyon"      -> pure RockyCanyon
          "rocky canyons"     -> pure RockyCanyon
          "cliff"             -> pure Cliff
          "cliffs"            -> pure Cliff
          t                   ->
            fail ("Unexpected value for terrain: " <> Text.unpack t)

instance {-# OVERLAPS #-} FromJSON ([Terrain] :: Type) where
  parseJSON :: Value -> Parser [Terrain]
  parseJSON =
    Aeson.withText "[Terrain]" $
        mapM (parseJSON @Terrain . String) . Text.splitOn ", "

-- TODO: Surely there's a way to generalize these kinds of instances?

instance FromJSON (SurfaceWaterPercent :: Type) where
  parseJSON :: Value -> Parser SurfaceWaterPercent
  parseJSON =
    Aeson.withText "SurfaceWaterPercent" $
      \case
        "unknown" -> pure UnknownPercentage
        val ->
          case Text.Read.double (Text.toLower val) of
            Right (percent, "") -> pure (Percentage percent)
            Right s ->
              fail ("Unexpected value for surface water: " <> show s)
            Left e -> fail e

instance FromJSON (Population :: Type) where
  parseJSON :: Value -> Parser Population
  parseJSON =
    Aeson.withText "Population" $
      \case
        "unknown" -> pure UnknownPopulation
        val ->
          case Text.Read.decimal (Text.toLower val) of
            Right (population, "") -> pure (Population population)
            Right s ->
              fail ("Unexpected value for population: " <> show s)
            Left e -> fail e

instance FromJSON (Planet :: Type) where
  parseJSON :: Value -> Parser Planet
  parseJSON =
    Aeson.withObject "Planet" $
      \val ->
        Planet
          <$> val .: "name"
          <*> val .: "rotation_period"
          <*> val .: "orbital_period"
          <*> val .: "diameter"
          <*> val .: "climate"
          <*> val .: "gravity"
          <*> val .: "terrain"
          <*> val .: "surface_water"
          <*> val .: "population"
          <*> val .: "residents"
          <*> val .: "films"
          <*> val .: "created"
          <*> val .: "edited"
          <*> val .: "url"

instance FromJSON ((Index Planet) :: Type) where
  parseJSON :: Value -> Parser (Index Planet)
  parseJSON =
    Aeson.withObject "Index Planet" $
      \val ->
        Index
          <$> val .: "count"
          <*> val .: "next"
          <*> val .: "previous"
          <*> val .: "results"

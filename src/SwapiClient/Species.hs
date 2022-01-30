module SwapiClient.Species where

--------------------------------------------------------------------------------

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson (withText, withObject)
import Data.Aeson.Types (FromJSON, Value, Parser, parseJSON)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (toLower, unpack)
import Data.Text.Read qualified as Text.Read (decimal)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Color (SkinColor, HairColor, EyeColor)
import SwapiClient.Id (HomeworldId, PersonId, FilmId, SpeciesId)
import SwapiClient.Page (Index (Index))

--------------------------------------------------------------------------------
-- Data types

newtype SpeciesName = SpeciesName Text
  deriving (Eq, Show)

data Designation
  = SentientDesignation
  | ReptilianDesignation
  deriving (Eq, Show)

data Classification
  = MammalClass
  | ArtificialClass
  | SentientClass
  | GastropodClass
  | ReptileClass
  | AmphibianClass
  deriving (Eq, Show)

data AverageHeight
  = Height Word
  | HeightNotApplicable
  deriving (Eq, Show)

data AverageLifespan
  = Lifespan Word
  | Indefinite
  | UnknownLifespan
  deriving (Eq, Show)

data Language
  = GalacticBasic
  | Shyriiwook
  | Huttese
  | Dosh
  | MonCalamarian
  | Ewokese
  | Sullutese
  | NoLanguage
  deriving (Eq, Show)

data Species = Species
  { spName :: SpeciesName
  , spClassification :: Classification
  , spDesignation :: Designation
  , spAverageHeight :: AverageHeight
  , spSkinColors :: [SkinColor]
  , spHairColors :: [HairColor]
  , spEyeColors :: [EyeColor]
  , spAverageLifespan :: AverageLifespan
  , spHomeworld :: HomeworldId
  , spLanguage :: Language
  , spPeople :: [PersonId]
  , spFilms :: [FilmId]
  , spCreatedAt :: UTCTime
  , spEditedAt :: UTCTime
  , spId :: SpeciesId
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (SpeciesName :: Type) where
  parseJSON :: Value -> Parser SpeciesName
  parseJSON = Aeson.withText "SpeciesName" (pure . SpeciesName)

instance FromJSON (Designation :: Type) where
  parseJSON :: Value -> Parser Designation
  parseJSON =
    Aeson.withText "Designation" $
      \case
        "sentient" -> pure SentientDesignation
        "reptilian" -> pure ReptilianDesignation
        _ -> fail "Unexpected value for species' designation"

instance FromJSON (Classification :: Type) where
  parseJSON :: Value -> Parser Classification
  parseJSON =
    Aeson.withText "Classification" $
      \case
        "mammal" -> pure MammalClass
        "artificial" -> pure ArtificialClass
        "sentient" -> pure SentientClass
        "gastropod" -> pure GastropodClass
        "reptile" -> pure ReptileClass
        "amphibian" -> pure AmphibianClass
        c ->
          fail ("Unexpected value for species' classification: " <> Text.unpack c)

instance FromJSON (AverageHeight :: Type) where
  parseJSON :: Value -> Parser AverageHeight
  parseJSON =
    Aeson.withText "AverageHeight" $
      \case
        "n/a" -> pure HeightNotApplicable
        val ->
          case Text.Read.decimal val of
            Right (avgHeight, "") ->
              pure (Height avgHeight)

            Right _ -> fail "Unexpected value for species' average height"
            Left e -> fail e

instance FromJSON (AverageLifespan :: Type) where
  parseJSON :: Value -> Parser AverageLifespan
  parseJSON =
    Aeson.withText "AverageLifespan" $
      \case
        "indefinite" -> pure Indefinite
        "unknown" -> pure UnknownLifespan
        val ->
          case Text.Read.decimal val of
            Right (avgLifespan, "") -> pure (Lifespan avgLifespan)
            Right _ -> fail "Unexpected value for species' average lifespan"
            Left e -> fail e

instance FromJSON (Language :: Type) where
  parseJSON :: Value -> Parser Language
  parseJSON =
    Aeson.withText "Language" $
      \val ->
        case Text.toLower val of
        -- FIXME: Have to accommodate this for swapi.dev's typo.
        "galatic basic" -> pure GalacticBasic
        "galactic basic" -> pure GalacticBasic
        "shyriiwook" -> pure Shyriiwook
        "huttese" -> pure Huttese
        "dosh" -> pure Dosh
        "mon calamarian" -> pure MonCalamarian
        "ewokese" -> pure Ewokese
        "sullutese" -> pure Sullutese
        "n/a" -> pure NoLanguage
        l -> fail ("Unexpected value for species' language: " <> Text.unpack l)

instance FromJSON (Species :: Type) where
  parseJSON :: Value -> Parser Species
  parseJSON =
    Aeson.withObject "Species" $
      \val ->
        Species
          <$> val .: "name"
          <*> val .: "classification"
          <*> val .: "designation"
          <*> val .: "average_height"
          <*> val .: "skin_colors"
          <*> val .: "hair_colors"
          <*> val .: "eye_colors"
          <*> val .: "average_lifespan"
          <*> val .: "homeworld"
          <*> val .: "language"
          <*> val .: "people"
          <*> val .: "films"
          <*> val .: "created"
          <*> val .: "edited"
          <*> val .: "url"

instance FromJSON (Index Species :: Type) where
  parseJSON :: Value -> Parser (Index Species)
  parseJSON =
    Aeson.withObject "Index Species" $
      \val ->
        Index
          <$> val .: "count"
          <*> val .: "next"
          <*> val .: "previous"
          <*> val .: "results"

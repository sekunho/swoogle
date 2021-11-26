module SwapiClient () where

import Data.Aeson
import qualified Data.Map as Map ()
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Data.Scientific as Scientific (toBoundedInteger)

newtype PersonName = PersonName Text
  deriving Show

data BirthYear
  = BBY Double
  | ABY Double
  | UnknownBirthYear
  deriving Show

-- TODO(sekun): Add other colors
data Color
  = Blue
  | Yellow
  | Red
  | Hazel
  | NoColor
  deriving Show

newtype HairColor = HairColor Color
  deriving Show

newtype SkinColor = SkinColor Color
  deriving Show

newtype EyeColor = EyeColor Color
  deriving Show

data Height
  = Height Int
  | UnknownHeight
  deriving Show

data Mass
  = Mass Double
  | UnknownMass
  deriving Show

data Gender
  = Male
  | Female
  deriving Show

newtype Homeworld = Homeworld Int
  deriving Show

-- TODO(sekun): Add other person attributes
data Person = Person
  { pName      :: PersonName            -- Name of person
  , pHeight    :: Height        -- Height of person can be Nothing
  , pMass      :: Mass            -- Mass of person can be Nothing
  , pHairColor :: HairColor
  , pSkinColor :: SkinColor
  , pEyeColor  :: EyeColor    -- Uh, eye color.
  , pBirthYear :: BirthYear  -- Relative to before/after Battle of Yavin
  , pGender    :: Gender           -- Gender according to SWAPI
  , pHomeworld :: Homeworld        -- TODO: Should this be something else?
  }
  deriving (Show)

-- HACK: Maybe there are better ways to handle unexpected inputs?

instance FromJSON PersonName where
  parseJSON (String name) = pure $ PersonName name
  parseJSON _ = error "ERROR: Unexpected type for person's name"

instance ToJSON PersonName where
  toJSON :: PersonName -> Value
  toJSON (PersonName name) = String name

instance FromJSON Height where
  parseJSON (String "unknown") = pure UnknownHeight
  parseJSON (String height) =
    case Text.Read.decimal height of
      Left e -> error $ mconcat ["ERROR: ", e]
      Right (numHeight, _) -> pure $ Height numHeight
  parseJSON _ = error "ERROR: Unexpected type for person's height"

instance ToJSON Height where
  toJSON height =
    case height of
      -- There's no `Integral a => a -> Text` apparently. So this is a hack for
      -- now. Relevant issue: https://github.com/haskell/text/issues/218
      Height n -> String . Text.pack . show $ n
      UnknownHeight -> String "unknown"

instance FromJSON Mass where
  parseJSON (String "unknown") = pure UnknownMass
  parseJSON (String mass) =
    case Text.Read.double mass of
      Left e -> error $ mconcat ["ERROR: ", e]
      Right (numMass, _) -> pure $ Mass numMass

  parseJSON _ = error "ERROR: Unexpected type for mass"

instance ToJSON Mass where
  toJSON mass =
    case mass of
      Mass numMass -> String . Text.pack . show $ numMass
      UnknownMass -> String "unknown"

-- TODO: Consider `DerivingVia`  for colors? A bit tiring to do by hand. :(
instance FromJSON HairColor where
  parseJSON (String hairColor) =
    case hairColor of
      "blue" -> pure (HairColor Blue)
      "yellow" -> pure (HairColor Yellow)
      "red" -> pure (HairColor Red)
      "hazel" -> pure (HairColor Hazel)
      "none" -> pure (HairColor NoColor)
      _ -> error "ERROR: Unexpected hair color value"

  parseJSON _ = error "ERROR: Unexpected hair color type"

instance ToJSON HairColor where
  toJSON (HairColor color) =
    case color of
      Blue -> String "blue"
      Yellow -> String "yellow"
      Red -> String "red"
      Hazel -> String "hazel"
      NoColor -> String "none"

instance FromJSON SkinColor where
  parseJSON (String skinColor) =
    case skinColor of
      "blue" -> pure (SkinColor Blue)
      "yellow" -> pure (SkinColor Yellow)
      "red" -> pure (SkinColor Red)
      "hazel" -> pure (SkinColor Hazel)
      "none" -> pure (SkinColor NoColor)
      _ -> error "ERROR: Unexpected skin color value"

  parseJSON _ = error "ERROR: Unexpected skin color type"

instance ToJSON SkinColor where
  toJSON (SkinColor color) =
    case color of
      Blue -> String "blue"
      Yellow -> String "yellow"
      Red -> String "red"
      Hazel -> String "hazel"
      NoColor -> String "none"

instance FromJSON EyeColor where
  parseJSON (String eyeColor) =
    case eyeColor of
      "blue" -> pure (EyeColor Blue)
      "yellow" -> pure (EyeColor Yellow)
      "red" -> pure (EyeColor Red)
      "hazel" -> pure (EyeColor Hazel)
      _ -> error "ERROR: Unexpected eye color value"

  parseJSON _ = error "ERROR: Unexpected type for eye color"

instance ToJSON EyeColor where
  toJSON :: EyeColor -> Value
  toJSON (EyeColor eyeColor) =
    case eyeColor of
      Blue -> String "blue"
      Yellow -> String "yellow"
      Red -> String "red"
      Hazel -> String "hazel"
      NoColor -> String "none"

instance FromJSON BirthYear where
  -- TODO(sekun): Add instance type signature
  parseJSON (String "unknown") = pure UnknownBirthYear
  parseJSON (String t) =
    case Text.Read.double t of
      Right (numYear, "BBY") -> pure $ BBY numYear
      Right (numYear, "ABY") -> pure $ ABY numYear
      Right (_, _) -> error "ERROR: Unexpected format for birth year"
      -- TODO: I should probably include the error from `double`.
      Left _ -> error "ERROR: Unexpected type for birth year"

  parseJSON _ = error "ERROR: Unexpected type for birth year"

-- TODO: If it's `*.0` then it would be cool to format it as just a whole number
instance ToJSON BirthYear where
  toJSON :: BirthYear -> Value
  toJSON (BBY years) = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years) = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON UnknownBirthYear = String "unknown"

instance FromJSON Gender where
  parseJSON (String gender) =
    case gender of
      "male" -> pure Male
      "female" -> pure Female
      _ -> error "ERROR: Unexpected value for gender"

  parseJSON _ = error "ERROR: Unexpected type for gender"

instance ToJSON Gender where
  toJSON gender =
    case gender of
      Male -> String "male"
      Female -> String "female"

instance FromJSON Homeworld where
  parseJSON (Number homeworldId) =
    case Scientific.toBoundedInteger homeworldId of
      Nothing -> error "ERROR: Homeworld ID must be a valid integer"
      Just intId -> pure $ Homeworld intId

  parseJSON _ = error "ERROR: Unexpected type for homeworld ID"

instance ToJSON Homeworld where
  toJSON (Homeworld homeworldId) = Number . fromIntegral $ homeworldId

instance FromJSON Person where
  parseJSON (Object v) =
    Person
      <$> v .: "name"
      <*> v .: "height"
      <*> v .: "mass"
      <*> v .: "hair_color"
      <*> v .: "skin_color"
      <*> v .: "eye_color"
      <*> v .: "birth_year"
      <*> v .: "gender"
      <*> v .: "homeworld"

  parseJSON _ = error "Bruh"

instance ToJSON Person where
  toJSON person =
    object
      [ "name" .= pName person,
        "height" .= pHeight person,
        "mass" .= pMass person,
        "hair_color" .= pHairColor person,
        "skin_color" .= pSkinColor person,
        "eye_color" .= pEyeColor person,
        "birth_year" .= pBirthYear person,
        "gender" .= pGender person,
        "homeworld" .= pHomeworld person
      ]

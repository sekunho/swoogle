module SwapiClient () where

import Data.Aeson
import qualified Data.Map as Map ()
import Data.Text (Text)
import qualified Data.Text as Text

data BirthYear
  = BBY Float
  | ABY Float
  | Unknown
  deriving (Show)

-- TODO(sekun): Add other eye colors
data EyeColor
  = Blue
  | Yellow
  | Red
  | Hazel
  deriving (Show)

-- TODO(sekun): Add other person attributes
data Person = Person
  { pName :: Text, -- Name of person
    pBirthYear :: BirthYear, -- Period relative to before or after Battle of Yavin
    pEyeColor :: EyeColor
  }
  deriving (Show)

instance FromJSON EyeColor where
  parseJSON (String eyeColor) =
    case eyeColor of
      "blue" -> pure Blue
      "yellow" -> pure Yellow
      "red" -> pure Red
      "hazel" -> pure Hazel
      _ -> error "ERROR: Unexpected eye color value"

instance ToJSON EyeColor where
  toJSON :: EyeColor -> Value
  toJSON eyeColor = case eyeColor of
    Blue -> String "blue"
    Yellow -> String "yellow"
    Red -> String "red"
    Hazel -> String "hazel"

instance FromJSON BirthYear where
  -- TODO(sekun): Add instance type signature
  parseJSON (String "unknown") = pure Unknown
  parseJSON (String t) =
    case Text.takeEnd 3 t of
      "BBY" ->
        let years :: Float
            years = read (Text.unpack $ Text.dropEnd 3 t)
         in pure . BBY $ years
      "ABY" ->
        let years :: Float
            years = read (Text.unpack $ Text.dropEnd 3 t)
         in pure . ABY $ years
      _ -> error "ERROR: Unexpected value for birth year"
  parseJSON _ = error "ERROR: Unexpected type for birth year"

instance ToJSON BirthYear where
  toJSON :: BirthYear -> Value
  toJSON (BBY years) = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years) = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON Unknown = String "unknown"

instance FromJSON Person where
  parseJSON (Object v) =
    Person
      <$> v .: "name"
      <*> v .: "birth_year"
      <*> v .: "eye_color"
  parseJSON _ = error "Bruh"

instance ToJSON Person where
  toJSON person =
    object
      [ "name" .= pName person,
        "birth_year" .= pBirthYear person,
        "eye_color" .= pEyeColor person
      ]

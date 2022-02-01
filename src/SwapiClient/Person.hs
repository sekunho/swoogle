{-# LANGUAGE FlexibleInstances #-}

module SwapiClient.Person
  ( BirthYear (BBY, ABY, UnknownBirthYear)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , Gender (Male, Female, Hermaphrodite, NoGender)
  , PersonName (PersonName)
  , Person
      ( pName
      , pHeight
      , pMass
      , pHairColor
      , pSkinColor
      , pEyeColor
      , pBirthYear
      , pGender
      , pHomeworldId
      , pFilmIds
      , pSpeciesIds
      , pVehicleIds
      , pStarshipIds
      , pCreatedAt
      , pEditedAt
      , pId
      )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson        qualified as Aeson (object, withObject, withText)
import Data.Aeson.Types  (FromJSON (parseJSON), KeyValue ((.=)), Parser,
                          ToJSON (toJSON), Value (String), (.:))

import Data.Kind         (Type)
import Data.Text         (Text)
import Data.Text         qualified as Text (filter, pack)
import Data.Text.Read    qualified as Text.Read (decimal, double)
import Data.Time         (UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Color (EyeColor, HairColor, SkinColor)

import SwapiClient.Id    (FilmId, PlanetId, PersonId, SpeciesId, StarshipId,
                          VehicleId)

import SwapiClient.Page  (Index (Index, iCount, iNextPage, iPreviousPage, iResults))

--------------------------------------------------------------------------------
-- Data types

data BirthYear
  = BBY Double
  | ABY Double
  | UnknownBirthYear
  deriving stock (Eq, Show)

data Height
  = Height Int
  | UnknownHeight
  deriving stock (Eq, Show)

data Mass
  = Mass Double
  | UnknownMass
  deriving stock (Eq, Show)

data Gender
  = Male
  | Female
  | Hermaphrodite
  | NoGender
  deriving stock (Eq, Show)

newtype PersonName = PersonName Text
  deriving stock (Eq, Show)

data Person = Person
  { pName        :: PersonName     -- Name of person
  , pHeight      :: Height         -- Height of person can be Nothing
  , pMass        :: Mass           -- Mass of person can be Nothing
  , pHairColor   :: [HairColor]
  , pSkinColor   :: [SkinColor]
  , pEyeColor    :: [EyeColor]     -- Uh, eye color.
  , pBirthYear   :: BirthYear      -- Relative to before/after Battle of Yavin
  , pGender      :: Gender         -- Gender according to SWAPI
  , pHomeworldId :: PlanetId    -- Homeworld IDs of character
  , pFilmIds     :: [FilmId]       -- Film IDs of character appearance
  , pSpeciesIds  :: [SpeciesId]
  , pVehicleIds  :: [VehicleId]
  , pStarshipIds :: [StarshipId]
  , pCreatedAt   :: UTCTime
  , pEditedAt    :: UTCTime
  , pId          :: PersonId
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (Height :: Type) where
  parseJSON :: Value -> Parser Height
  parseJSON =
   Aeson.withText "Height" $
      \case
        "unknown" -> pure UnknownHeight
        strHeight ->
          case Text.Read.decimal strHeight of
            Right (numHeight, "") -> pure (Height numHeight)
            Left e                -> fail e
            _                     -> fail "Unexpected format for height"

instance ToJSON (Height :: Type) where
  toJSON :: Height -> Value
  toJSON height =
    case height of
      -- There's no `Integral a => a -> Text` apparently. So this is a hack for
      -- now. Relevant issue: https://github.com/haskell/text/issues/218
      Height n      -> String . Text.pack . show $ n
      UnknownHeight -> String "unknown"

instance FromJSON (Mass :: Type) where
  parseJSON :: Value -> Parser Mass
  parseJSON =
   Aeson.withText "Mass" $
     \case
       "unknown" -> pure UnknownMass

       mass' ->
         case Text.Read.double $ Text.filter (/= ',') mass' of
           Right (numMass, "") -> pure . Mass $ numMass
           Right (_, _)        -> fail "ERROR: Unexpected format"
           Left e              -> fail e

instance ToJSON (Mass :: Type) where
  toJSON :: Mass -> Value
  toJSON mass =
    case mass of
      Mass numMass -> String . Text.pack . show $ numMass
      UnknownMass  -> String "unknown"

instance FromJSON (BirthYear :: Type) where
  parseJSON :: Value -> Parser BirthYear
  parseJSON =
   Aeson.withText "BirthYear" $
      \birthYear ->
        case birthYear of
          "unknown" -> pure UnknownBirthYear
          _ ->
            case Text.Read.double birthYear of
              Right (numYear, "BBY") -> pure $ BBY numYear
              Right (numYear, "ABY") -> pure $ ABY numYear
              Right (_, _) -> fail "ERROR: Unexpected format for birth year"
              Left _ -> fail "ERROR: Unexpected type for birth year"

instance ToJSON (BirthYear :: Type) where
  toJSON :: BirthYear -> Value
  -- FIXME(sekun): Maybe use `showt` rather than `Text.pack . show`?
  toJSON (BBY years)      = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years)      = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON UnknownBirthYear = String "unknown"

instance FromJSON (Gender :: Type) where
  parseJSON :: Value -> Parser Gender
  parseJSON =
   Aeson.withText "Gender" $
      \case
        "male"          -> pure Male
        "female"        -> pure Female
        "hermaphrodite" -> pure Hermaphrodite
        "none"          -> pure NoGender
        "n/a"           -> pure NoGender
        _               -> fail "ERROR: Unexpected value for gender"

instance ToJSON (Gender :: Type) where
  toJSON :: Gender -> Value
  toJSON gender =
    case gender of
      Male          -> String "male"
      Female        -> String "female"
      Hermaphrodite -> String "hermaphrodite"
      NoGender      -> String "n/a"

instance FromJSON (PersonName :: Type) where
  parseJSON :: Value -> Parser PersonName
  parseJSON =
   Aeson.withText "PersonName"
      $ \name ->
          pure $ PersonName name

instance ToJSON (PersonName :: Type) where
  toJSON :: PersonName -> Value
  toJSON (PersonName name) = String name

instance FromJSON (Person :: Type) where
  parseJSON :: Value -> Parser Person
  parseJSON =
   Aeson.withObject "Person" $
      \objPerson ->
        Person
          <$> objPerson .: "name"
          <*> objPerson .: "height"
          <*> objPerson .: "mass"
          <*> objPerson .: "hair_color"
          <*> objPerson .: "skin_color"
          <*> objPerson .: "eye_color"
          <*> objPerson .: "birth_year"
          <*> objPerson .: "gender"
          <*> objPerson .: "homeworld"
          <*> objPerson .: "films"
          <*> objPerson .: "species"
          <*> objPerson .: "vehicles"
          <*> objPerson .: "starships"
          <*> objPerson .: "created"
          <*> objPerson .: "edited"
          <*> objPerson .: "url"

instance ToJSON (Person :: Type) where
  toJSON :: Person -> Value
  toJSON person =
    Aeson.object
      [ "name"       .= pName person
      , "height"     .= pHeight person
      , "mass"       .= pMass person
      , "hair_color" .= pHairColor person
      , "skin_color" .= pSkinColor person
      , "eye_color"  .= pEyeColor person
      , "birth_year" .= pBirthYear person
      , "gender"     .= pGender person
      , "homeworld"  .= pHomeworldId person
      , "films"      .= pFilmIds person
      , "species"    .= pSpeciesIds person
      , "vehicles"   .= pVehicleIds person
      , "starships"  .= pStarshipIds person
      , "created"    .= pCreatedAt person
      , "edited"     .= pEditedAt person
      , "url"        .= pId person
      ]

instance FromJSON (Index Person :: Type) where
  parseJSON :: Value -> Parser (Index Person)
  parseJSON =
    Aeson.withObject "Index" $
      \indexObject ->
        Index
          <$> indexObject .: "count"
          <*> indexObject .: "next"
          <*> indexObject .: "previous"
          <*> indexObject .: "results"

instance ToJSON (Index Person :: Type) where
  toJSON :: Index Person -> Value
  toJSON indexObject =
    Aeson.object
      [ "count"     .= iCount indexObject
      , "next"      .= iNextPage indexObject
      , "previous"  .= iPreviousPage indexObject
      , "results"   .= iResults indexObject
      ]


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

import Data.Aeson qualified as Aeson
  ( object
  , withObject
  , withText
  )
import Data.Aeson.Types
    ( KeyValue ((.=))
    , ToJSON (toJSON)
    , FromJSON (parseJSON)
    , Value (String)
    , Parser
    , (.:)
    )
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Data.Text.Read qualified as Text.Read (decimal, double)
import Data.Time (UTCTime)
import Data.Kind (Type)

--------------------------------------------------------------------------------

import SwapiClient.Color
  ( HairColors
  , SkinColors
  , EyeColor
  )

import SwapiClient.Id
  ( FilmId
  , HomeworldId
  , SpeciesId
  , VehicleId
  , StarshipId
  , PersonId
  )

--------------------------------------------------------------------------------
-- Data types

data BirthYear
  = BBY Double
  | ABY Double
  | UnknownBirthYear
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
  | Hermaphrodite
  | NoGender
  deriving Show

newtype PersonName = PersonName Text
  deriving Show

-- TODO(sekun): Add other person attributes
data Person = Person
  { pName             :: PersonName     -- Name of person
  , pHeight           :: Height         -- Height of person can be Nothing
  , pMass             :: Mass           -- Mass of person can be Nothing
  , pHairColor        :: HairColors
  , pSkinColor        :: SkinColors
  , pEyeColor         :: EyeColor       -- Uh, eye color.
  , pBirthYear        :: BirthYear      -- Relative to before/after Battle of Yavin
  , pGender           :: Gender         -- Gender according to SWAPI
  , pHomeworldId      :: HomeworldId    -- Homeworld IDs of character
  , pFilmIds          :: [FilmId]       -- Film IDs of character appearance
  , pSpeciesIds       :: [SpeciesId]
  , pVehicleIds       :: [VehicleId]
  , pStarshipIds      :: [StarshipId]
  , pCreatedAt        :: UTCTime
  , pEditedAt         :: UTCTime
  , pId               :: PersonId
  }
  deriving Show

data Page
  = NextPage Int
  | PreviousPage Int
  | NoPageAvailable
  deriving Show

data PersonIndex = PersonIndex
  { plCount :: Int
--  , plCurrentPage :: Int
--  , plNextPage :: Page
--  , plPreviousPage :: Page
  , plResults :: [Person]
  }
  deriving Show

--------------------------------------------------------------------------------
-- INSTANCES
-- TODO: Implement `FromJSON` and `ToJSON` instances for `SkinColor`
-- TODO: Implement `FromJSON` and `ToJSON` instances for `EyeColor`

instance FromJSON (Height :: Type) where
  parseJSON :: Value -> Parser Height
  parseJSON =
   Aeson.withText "Height" $
      \case
        "unknown" -> pure UnknownHeight
        strHeight ->
          case Text.Read.decimal strHeight of
            Right (numHeight, "") -> pure (Height numHeight)
            Left e -> fail e
            _ -> fail "Unexpected format for height"

instance ToJSON (Height :: Type) where
  toJSON :: Height -> Value
  toJSON height =
    case height of
      -- There's no `Integral a => a -> Text` apparently. So this is a hack for
      -- now. Relevant issue: https://github.com/haskell/text/issues/218
      Height n -> String . Text.pack . show $ n
      UnknownHeight -> String "unknown"

instance FromJSON (Mass :: Type) where
  parseJSON :: Value -> Parser Mass
  parseJSON =
   Aeson.withText "Mass"
      $ \mass ->
          case Text.Read.double mass of
            Left e -> fail e
            Right (numMass, "") -> pure . Mass $ numMass
            Right (_, _) -> fail "ERROR: Unexpected format"

instance ToJSON (Mass :: Type) where
  toJSON :: Mass -> Value
  toJSON mass =
    case mass of
      Mass numMass -> String . Text.pack . show $ numMass
      UnknownMass -> String "unknown"


instance FromJSON (BirthYear :: Type) where
  -- TODO(sekun): Add instance type signature
  parseJSON :: Value -> Parser BirthYear
  parseJSON =
   Aeson.withText "BirthYear" $
      \birthYear ->
        case Text.Read.double birthYear of
          Right (numYear, "BBY") -> pure $ BBY numYear
          Right (numYear, "ABY") -> pure $ ABY numYear
          Right (_, _) -> fail "ERROR: Unexpected format for birth year"
          Left _ -> fail "ERROR: Unexpected type for birth year"

instance ToJSON (BirthYear :: Type) where
  toJSON :: BirthYear -> Value
  -- FIXME(sekun): Maybe use `showt` rather than `Text.pack . show`?
  toJSON (BBY years) = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years) = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON UnknownBirthYear = String "unknown"

instance FromJSON (Gender :: Type) where
  parseJSON :: Value -> Parser Gender
  parseJSON =
   Aeson.withText "Gender" $
      \case
        "male" -> pure Male
        "female" -> pure Female
        "hermaphrodite" -> pure Hermaphrodite
        "none" -> pure NoGender
        "n/a" -> pure NoGender
        _ -> fail "ERROR: Unexpected value for gender"

instance ToJSON (Gender :: Type) where
  toJSON :: Gender -> Value
  toJSON gender =
    case gender of
      Male -> String "male"
      Female -> String "female"
      Hermaphrodite -> String "hermaphrodite"
      NoGender -> String "n/a"

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

--instance FromJSON (Page :: Type) where
--  parseJSON :: Value -> Parser Page
--  parseJSON =
--    Aeson.withText "Page" $
--      \pageUrl -> _

-- instance FromJSON (PersonIndex :: Type) where
--   parseJSON :: Value -> Parser PersonIndex
--   parseJSON =
--     Aeson.withObject "PersonIndex" $
--       \indexObject ->
--         PersonIndex
--           <$> indexObject .: "count"
--           <*> indexObject .: "next"
--           <*> indexObject .: "previous"
--           <*> indexObject .: "results"
--
-- instance ToJSON (PersonIndex :: Type) where
--   toJSON :: PersonIndex -> Value
--   toJSON indexObject =
--     Aeson.object
--       [ "count" .= plCount indexObject
--       , "next" .= plNextPage indexObject
--       , "previous" .= plPreviousPage indexObject
--       , "results" .= plResults indexObject
--       ]

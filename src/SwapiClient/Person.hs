module SwapiClient.Person
  ( BirthYear (BBY, ABY, UnknownBirthYear)
  , HairColors (HairColors)
  , HairColor
    ( AuburnHair
    , BlackHair
    , BlondHair
    , BrownHair
    , GreyHair
    , NoHairColor
    , WhiteHair
    )
  , SkinColor
    ( BlueSkin
    , BrownSkin
    , BrownMottleSkin
    , DarkSkin
    , FairSkin
    , GoldSkin
    , GreenSkin
    , GreenTanSkin
    , GreySkin
    , LightSkin
    , MetalSkin
    , MottledGreenSkin
    , OrangeSkin
    , PaleSkin
    , RedSkin
    , SilverSkin
    , TanSkin
    , UnknownSkinColor
    , WhiteSkin
    , YellowSkin
    )
  , EyeColor
    ( BlackEye
    , BlueEye
    , BlueGreyEye
    , GoldEye
    , HazelEye
    , OrangeEye
    , PinkEye
    , RedEye
    , UnknownEyeColor
    , YellowEye
    )
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , Gender (Male, Female)
  , PersonName (PersonName)
  , Person
      ( pName
      , pHeight
      , pMass
      , pHairColor
    --  , pSkinColor
    --  , pEyeColor
      , pBirthYear
      , pGender
      , pHomeworldId
      , pFilmIds )
  , lukeSkywalker
  ) where

import Data.Aeson
    ( object,
      (.:),
      withObject,
      withText,
      KeyValue((.=)),
      ToJSON(toJSON),
      FromJSON(parseJSON),
      Value(String) )
import Data.List qualified as List (foldl')
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn)
import Data.Text.Read qualified as Text.Read (decimal, double)
import Data.Kind (Type)

--------------------------------------------------------------------------------

import SwapiClient.Id
  ( FilmId
  , HomeworldId
  , SpeciesId
  , VehicleId
  , StarshipId
  )

import SwapiClient.Id qualified as Id
  ( lukeFilmIds
  , lukeHomeworldId
  , lukeSpeciesIds
  , lukeVehicleIds
  , lukeStarshipIds
  )

--------------------------------------------------------------------------------
-- DATA TYPES

data BirthYear
  = BBY Double
  | ABY Double
  | UnknownBirthYear
  deriving Show

data HairColor
  = AuburnHair
  | BlackHair
  | BlondHair
  | BrownHair
  | GreyHair
  | NoHairColor
  | WhiteHair
  deriving Show

newtype HairColors = HairColors [HairColor]
  deriving Show

data SkinColor
  = BlueSkin
  | BrownSkin
  | BrownMottleSkin
  | DarkSkin
  | FairSkin
  | GoldSkin
  | GreenSkin
  | GreenTanSkin
  | GreySkin
  | LightSkin
  | MetalSkin
  | MottledGreenSkin
  | OrangeSkin
  | PaleSkin
  | RedSkin
  | SilverSkin
  | TanSkin
  | UnknownSkinColor
  | WhiteSkin
  | YellowSkin
  deriving Show

data EyeColor
  = BlackEye
  | BlueEye
  | BlueGreyEye
  | GoldEye
  | HazelEye
  | OrangeEye
  | PinkEye -- lmao
  | RedEye
  | UnknownEyeColor
  | YellowEye
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

newtype PersonName = PersonName Text
  deriving Show

-- TODO(sekun): Add other person attributes
data Person = Person
  { pName             :: PersonName     -- Name of person
  , pHeight           :: Height         -- Height of person can be Nothing
  , pMass             :: Mass           -- Mass of person can be Nothing
  , pHairColor        :: HairColors
  --, pSkinColor        :: [SkinColor]
  --, pEyeColor         :: EyeColor       -- Uh, eye color.
  , pBirthYear        :: BirthYear      -- Relative to before/after Battle of Yavin
  , pGender           :: Gender         -- Gender according to SWAPI
  , pHomeworldId      :: HomeworldId    -- Homeworld IDs of character
  , pFilmIds          :: [FilmId]       -- Film IDs of character appearance
  , pSpeciesIds       :: [SpeciesId]
  , pVehicleIds       :: [VehicleId]
  , pStarshipIds      :: [StarshipId]
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- INSTANCES
-- TODO: Implement `FromJSON` and `ToJSON` instances for `SkinColor`
-- TODO: Implement `FromJSON` and `ToJSON` instances for `EyeColor`

instance FromJSON (Height :: Type) where
  parseJSON =
    withText "Height" $
      \case
        "unknown" -> pure UnknownHeight
        strHeight ->
          case Text.Read.decimal strHeight of
            Right (numHeight, "") -> pure (Height numHeight)
            Left e -> fail e
            _ -> fail "Unexpected format for height"

instance ToJSON (Height :: Type) where
  toJSON height =
    case height of
      -- There's no `Integral a => a -> Text` apparently. So this is a hack for
      -- now. Relevant issue: https://github.com/haskell/text/issues/218
      Height n -> String . Text.pack . show $ n
      UnknownHeight -> String "unknown"

instance FromJSON (Mass :: Type) where
  parseJSON =
    withText "Mass"
      $ \mass ->
          case Text.Read.double mass of
            Left e -> fail e
            Right (numMass, "") -> pure . Mass $ numMass
            Right (_, _) -> fail "ERROR: Unexpected format"

instance ToJSON (Mass :: Type) where
  toJSON mass =
    case mass of
      Mass numMass -> String . Text.pack . show $ numMass
      UnknownMass -> String "unknown"

-- TODO(sekun): Consider `DerivingVia`  for colors? A bit tiring to do by hand. :(
instance FromJSON (HairColors :: Type) where
  parseJSON =
    withText "HairColor"
      $ \hairText ->
          let hairColors :: Either String [HairColor]
              hairColors = mapM textToHairColor . Text.splitOn ", " $ hairText
          in
            case hairColors of
              Right hc -> pure . HairColors $ hc
              Left e -> fail e

instance ToJSON (HairColors :: Type) where
  toJSON (HairColors hairColors) =
    String . colorsToText $ hairColors
    where
      colorsToText :: [HairColor] -> Text
      colorsToText =
        List.foldl'
          (\hairColorsText color ->
             if hairColorsText == ""
             then hairColorToText color
             else mconcat [hairColorsText, ", ", hairColorToText color])
          mempty

--instance FromJSON (SkinColor :: Type) where
--  parseJSON = withText "SkinColor" $
--    \case
--      "blue" -> pure (SkinColor Blue)
--      "yellow" -> pure (SkinColor Yellow)
--      "red" -> pure (SkinColor Red)
--      "hazel" -> pure (SkinColor Hazel)
--      "none" -> pure (SkinColor NoColor)
--      _ -> fail "ERROR: Unexpected skin color value"
--
--instance ToJSON (SkinColor :: Type) where
--  toJSON (SkinColor color) =
--    case color of
--      Blue -> String "blue"
--      Yellow -> String "yellow"
--      Red -> String "red"
--      Hazel -> String "hazel"
--      NoColor -> String "none"
--
--instance FromJSON (EyeColor :: Type) where
--  parseJSON =
--    withText "EyeColor" $
--      \case
--        "blue" -> pure (EyeColor Blue)
--        "yellow" -> pure (EyeColor Yellow)
--        "red" -> pure (EyeColor Red)
--        "hazel" -> pure (EyeColor Hazel)
--        _ -> fail "ERROR: Unexpected eye color value"
--
--instance ToJSON (EyeColor :: Type) where
--  toJSON :: EyeColor -> Value
--  toJSON (EyeColor eyeColor) =
--    case eyeColor of
--      Blue -> String "blue"
--      Yellow -> String "yellow"
--      Red -> String "red"
--      Hazel -> String "hazel"
--      NoColor -> String "none"

instance FromJSON (BirthYear :: Type) where
  -- TODO(sekun): Add instance type signature
  parseJSON =
    withText "BirthYear" $
      \birthYear ->
        case Text.Read.double birthYear of
          Right (numYear, "BBY") -> pure $ BBY numYear
          Right (numYear, "ABY") -> pure $ ABY numYear
          Right (_, _) -> fail "ERROR: Unexpected format for birth year"
          Left _ -> fail "ERROR: Unexpected type for birth year"

-- TODO(sekun): If it's `*.0` then it would be cool to format it as just a whole number
instance ToJSON (BirthYear :: Type) where
  toJSON :: BirthYear -> Value
  -- FIXME(sekun): Maybe use `showt` rather than `Text.pack . show`?
  toJSON (BBY years) = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years) = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON UnknownBirthYear = String "unknown"

instance FromJSON (Gender :: Type) where
  parseJSON =
    withText "Gender" $
      \case
        "male" -> pure Male
        "female" -> pure Female
        _ -> fail "ERROR: Unexpected value for gender"

instance ToJSON (Gender :: Type) where
  toJSON gender =
    case gender of
      Male -> String "male"
      Female -> String "female"

instance FromJSON (PersonName :: Type) where
  parseJSON =
    withText "PersonName"
      $ \name ->
          pure $ PersonName name

instance ToJSON (PersonName :: Type) where
  toJSON :: PersonName -> Value
  toJSON (PersonName name) = String name

instance FromJSON (Person :: Type) where
  parseJSON =
    withObject "Person" $
      \objPerson ->
        Person
          <$> objPerson .: "name"
          <*> objPerson .: "height"
          <*> objPerson .: "mass"
          <*> objPerson .: "hair_color"
        --  <*> objPerson .: "skin_color"
        --  <*> objPerson .: "eye_color"
          <*> objPerson .: "birth_year"
          <*> objPerson .: "gender"
          <*> objPerson .: "homeworld"
          <*> objPerson .: "films"
          <*> objPerson .: "species"
          <*> objPerson .: "vehicles"
          <*> objPerson .: "starships"

instance ToJSON (Person :: Type) where
  toJSON person =
    object
      [ "name"       .= pName person
      , "height"     .= pHeight person
      , "mass"       .= pMass person
      , "hair_color" .= pHairColor person
 --     , "skin_color" .= pSkinColor person
 --     , "eye_color"  .= pEyeColor person
      , "birth_year" .= pBirthYear person
      , "gender"     .= pGender person
      , "homeworld"  .= pHomeworldId person
      , "films"      .= pFilmIds person
      , "species"    .= pSpeciesIds person
      , "vehicles"   .= pVehicleIds person
      , "starships"  .= pStarshipIds person
      ]

--------------------------------------------------------------------------------
-- FUNCTIONS

lukeSkywalker :: Person
lukeSkywalker =
  Person
    { pName        = PersonName "Luke Skywalker"
    , pHeight      = Height 172
    , pMass        = Mass 77
    , pHairColor   = HairColors [BlondHair, BrownHair]
--    , pSkinColor   = SkinColor Blue
--    , pEyeColor    = EyeColor Blue
    , pBirthYear   = BBY 19
    , pGender      = Male
    , pHomeworldId = Id.lukeHomeworldId
    , pFilmIds     = Id.lukeFilmIds
    , pSpeciesIds  = Id.lukeSpeciesIds
    , pVehicleIds  = Id.lukeVehicleIds
    , pStarshipIds = Id.lukeStarshipIds
    }

--------------------------------------------------------------------------------
-- Functions

textToHairColor :: Text -> Either String HairColor
textToHairColor hct = case hct of
  "auburn"     -> Right AuburnHair
  "black"      -> Right BlackHair
  "blond"      -> Right BlondHair
  "brown"      -> Right BrownHair
  "grey"       -> Right GreyHair
  "white"      -> Right WhiteHair
  "n/a"        -> Right NoHairColor
  "none"       -> Right NoHairColor
  _            -> Left "ERROR: Invalid hair color value/format"

hairColorToText :: HairColor -> Text
hairColorToText hairColor = case hairColor of
  AuburnHair -> "auburn"
  BlackHair -> "black"
  BlondHair -> "blond"
  BrownHair -> "brown"
  GreyHair -> "grey"
  NoHairColor -> "none"
  WhiteHair -> "white"

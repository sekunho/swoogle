module SwapiClient.Starship
  ( Starship
    ( Starship
    , sName
    , sModel
    , sManufacturer
    , sCost
    , sLength
    , sMaxAtmospheringSpeed
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
  , MaxAtmospheringSpeed (MaxSpeed, MASNotApplicable)
  , CostInCredits (Amount, UnknownCost)
  , Manufacturer (Manufacturer)
  , StarshipModel (StarshipModel)
  , StarshipName (StarshipName)
  , Consumable (Week, Month, Year)
  , RequiredCrew (CrewRange, CrewAmount)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, (.=), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value (String))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (split, toLower)
import Data.Text.Read qualified as Text.Read (decimal, double)
import TextShow (TextShow)
import TextShow qualified as Text.Show (showt)
import Data.Time (UTCTime)
import GHC.Stack (HasCallStack)

--------------------------------------------------------------------------------

import SwapiClient.Person (Person)
import SwapiClient.Film (Film)
import SwapiClient.Id (StarshipId)

--------------------------------------------------------------------------------
-- Data types

data RequiredCrew
  = CrewRange Word Word
  | CrewAmount Word
  deriving (Eq, Show)

data Consumable
  = Week Word
  | Month Word
  | Year Word
  deriving (Eq, Show)

newtype StarshipName = StarshipName Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype StarshipModel = StarshipModel Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype Manufacturer = Manufacturer Text
  deriving stock (Eq, Show)
  deriving newtype TextShow

data CostInCredits
  = Amount Word
  | UnknownCost
  deriving (Eq, Show)

data MaxAtmospheringSpeed
  = MaxSpeed Word
  | MASNotApplicable
  deriving (Eq, Show)

data PassengerLimit
  = PassengerLimit Word
  | PLNotApplicable
  deriving (Eq, Show)

newtype StarshipLength = StarshipLength Double
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype CargoCapacity = CargoCapacity Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype HyperdriveRating = HyperdriveRating Double
  deriving stock (Eq, Show)
  deriving newtype TextShow

newtype MaxMegalight = MaxMegalight Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

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
  { sName                 :: StarshipName
  , sModel                :: StarshipModel
  , sManufacturer         :: Manufacturer
  , sCost                 :: CostInCredits
  , sLength               :: StarshipLength
  , sMaxAtmospheringSpeed :: MaxAtmospheringSpeed
  , sRequiredCrew         :: RequiredCrew
  , sPassengerLimit       :: PassengerLimit
  , sCargoCapacity        :: CargoCapacity
  , sConsumables          :: Consumable
  , sHyperdriveRating     :: HyperdriveRating
  , sMaxMegalight         :: MaxMegalight
  , sStarshipClass        :: StarshipClass
  , sPilots               :: [Person]
  , sFilms                :: [Film]
  , sCreatedAt            :: UTCTime
  , sEditedAt             :: UTCTime
  , sId                   :: StarshipId
  }

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (StarshipName :: Type) where
  parseJSON :: Value -> Parser StarshipName
  parseJSON = Aeson.withText "StarshipName" (pure . StarshipName)

instance ToJSON (StarshipName :: Type) where
  toJSON :: StarshipName -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (StarshipModel :: Type) where
  parseJSON :: Value -> Parser StarshipModel
  parseJSON = Aeson.withText "StarshipModel" (pure . StarshipModel)

instance ToJSON (StarshipModel :: Type) where
  toJSON :: StarshipModel -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (Manufacturer :: Type) where
  parseJSON :: Value -> Parser Manufacturer
  parseJSON = Aeson.withText "Manufacturer" (pure . Manufacturer)

instance ToJSON (Manufacturer :: Type) where
  toJSON :: Manufacturer -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (CostInCredits :: Type) where
  parseJSON :: Value -> Parser CostInCredits
  parseJSON = Aeson.withText "CostInCredits" $
    \case
      "unknown" ->
        pure UnknownCost

      costText ->
        -- TODO: I unfortunately do not know a better way to parse `Text`.
        case Text.Read.decimal costText of
          Right (amount, "") ->
            pure $ Amount amount

          Right _ ->
            fail "Unexpected format for cost"

          Left e ->
            fail e

instance ToJSON (CostInCredits :: Type) where
  toJSON :: CostInCredits -> Value
  toJSON = String .
    \case
      Amount amount ->
        Text.Show.showt amount

      UnknownCost ->
        "unknown"

instance FromJSON (StarshipLength :: Type) where
  parseJSON :: Value -> Parser StarshipLength
  parseJSON =
    Aeson.withText "StarshipLength" $
      \lengthText ->
        -- TODO: I unfortunately do not know a better way to parse `Text`.
        case Text.Read.double lengthText of
          Right (starshipLength, "") ->
            pure $ StarshipLength starshipLength

          Right _ ->
            fail "Unexpected format for StarshipLength"

          Left e ->
            fail e

instance ToJSON (StarshipLength :: Type) where
  toJSON :: StarshipLength -> Value
  toJSON =  String . Text.Show.showt

instance FromJSON (MaxAtmospheringSpeed :: Type) where
  parseJSON :: Value -> Parser MaxAtmospheringSpeed
  parseJSON =
    Aeson.withText "MaxAtmospheringSpeed" $
      \case
        "n/a" ->
          pure MASNotApplicable

        iAmSpeed ->
          case Text.Read.decimal iAmSpeed of
            Right (speed, _) ->
              pure (MaxSpeed speed)

            Left e ->
              fail e

instance ToJSON (MaxAtmospheringSpeed :: Type) where
  toJSON :: MaxAtmospheringSpeed -> Value
  toJSON = String .
    \case
      MaxSpeed maxSpeed ->
        Text.Show.showt maxSpeed <> "km"

      MASNotApplicable ->
        "n/a"

instance FromJSON (RequiredCrew :: Type) where
  parseJSON :: Value -> Parser RequiredCrew
  parseJSON =
    Aeson.withText "RequiredCrew" $
      \res ->
        case parseAmount res of
          [minCrew, maxCrew] ->
            pure (CrewRange minCrew maxCrew)

          [crewAmount] ->
            pure (CrewAmount crewAmount)

          _ ->
            fail "Unexpected format for crew amount"
    where
      pluckWord :: HasCallStack => Either String (Word, Text) -> Word
      pluckWord =
        \case
          Right (speed, _) ->
            speed

          Left e ->
            error e

      parseAmount :: Text -> [Word]
      parseAmount = map (pluckWord . Text.Read.decimal) . Text.split (== '-')

instance ToJSON (RequiredCrew :: Type) where
  toJSON :: RequiredCrew -> Value
  toJSON = String .
    \case
      CrewRange minCrew maxCrew ->
        Text.Show.showt minCrew <> "-" <> Text.Show.showt maxCrew

      CrewAmount amount ->
        Text.Show.showt amount

instance FromJSON (PassengerLimit :: Type) where
  parseJSON :: Value -> Parser PassengerLimit
  parseJSON =
    Aeson.withText "PassengerLimit" $
      \case
        "n/a" ->
          pure PLNotApplicable

        limitText ->
          case Text.Read.decimal limitText of
            Right (passengerLimit, "") ->
              pure (PassengerLimit passengerLimit)

            Right _ ->
              fail "Unexpected format for `passengers`"

            Left e ->
              fail e

instance ToJSON (PassengerLimit :: Type) where
  toJSON :: PassengerLimit -> Value
  toJSON = String .
    \case
      PassengerLimit pLimit ->
        Text.Show.showt pLimit

      PLNotApplicable ->
        "n/a"

instance FromJSON (CargoCapacity :: Type) where
  parseJSON :: Value -> Parser CargoCapacity
  parseJSON =
    Aeson.withText "CargoCapacity" $
      \val ->
        case Text.Read.decimal val of
          Right (cargoCapacity, "") ->
            pure (CargoCapacity cargoCapacity)

          Right _ ->
            fail "Unexpected format for `cargo_capacity`."

          Left e ->
            fail e

instance ToJSON (CargoCapacity :: Type) where
  toJSON :: CargoCapacity -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (Consumable :: Type) where
  parseJSON :: Value -> Parser Consumable
  parseJSON =
    Aeson.withText "Consumable" $
      \val ->
        case Text.Read.decimal val of
          Right (timeLength, "week") ->
            pure (Week timeLength)

          Right (timeLength, "weeks") ->
            pure (Week timeLength)

          Right (timeLength, "month") ->
            pure (Month timeLength)

          Right (timeLength, "months") ->
            pure (Month timeLength)

          Right (timeLength, "year") ->
            pure (Year timeLength)

          Right (timeLength, "years") ->
            pure (Year timeLength)

          Right _ ->
            fail "Unexpected format for `consumables`"

          Left e ->
            fail e

instance ToJSON (Consumable :: Type) where
  toJSON :: Consumable -> Value
  toJSON = String .
    \case
      Week timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " week"

      Month timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " month"

      Year timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " year"

    where
      appendS :: Word -> Text -> Text
      appendS timeLength txt
        | timeLength > 1 = txt <> "s"
        | otherwise = txt

instance FromJSON (HyperdriveRating :: Type) where
  parseJSON :: Value -> Parser HyperdriveRating
  parseJSON =
    Aeson.withText "HyperdriveRating" $
      \val ->
        case Text.Read.double val of
          Right (rating, "") ->
            pure (HyperdriveRating rating)

          Right _ ->
            fail "Unexpected format for `hyperdrive_rating`"

          Left e ->
            fail e

instance ToJSON (HyperdriveRating :: Type) where
  toJSON :: HyperdriveRating -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (MaxMegalight :: Type) where
  parseJSON :: Value -> Parser MaxMegalight
  parseJSON =
    Aeson.withText "MaxMegalight" $
      \val ->
        case Text.Read.decimal val of
          Right (maxMegalight, "") ->
            pure (MaxMegalight maxMegalight)

          Right _ ->
            fail "Unexpected format for `MGLT`"

          Left e ->
            fail e

instance ToJSON (MaxMegalight :: Type) where
  toJSON :: MaxMegalight -> Value
  toJSON = String . Text.Show.showt

instance FromJSON (StarshipClass :: Type) where
  parseJSON :: Value -> Parser StarshipClass
  parseJSON =
    Aeson.withText "StarshipClass" $
      \val ->
        case Text.toLower val of
          "corvette" ->
            pure Corvette

          "star destroyer" ->
            pure StarDestroyer

          "landing craft" ->
            pure LandingCraft

          "deep space mobile battlestation" ->
            pure DeepSpaceMobileBattlestation

          "light freighter" ->
            pure LightFreighter

          "assault starfighter" ->
            pure AssaultStarfighter

          "starfighter" ->
            pure Starfighter

          "star dreadnought" ->
            pure StarDreadnought

          "medium transport" ->
            pure MediumTransport

          _ ->
            fail "Unexpected value for `starship_class`."

instance ToJSON (StarshipClass :: Type) where
  toJSON :: StarshipClass -> Value
  toJSON = String .
    \case
      Corvette ->
        "Corvette"

      StarDestroyer ->
        "Star Destroyer"

      LandingCraft ->
        "Landing Craft"

      DeepSpaceMobileBattlestation ->
        "Deep Space Mobile Battlestation"

      LightFreighter ->
        "Light Freighter"

      AssaultStarfighter ->
        "Assault Starfighter"

      Starfighter ->
        "Starfighter"

      StarDreadnought ->
        "Star Dreadnought"

      MediumTransport ->
        "Medium Transport"

instance FromJSON (Starship :: Type) where
  parseJSON :: Value -> Parser Starship
  parseJSON =
    Aeson.withObject "Starship" $
      \starshipObj ->
        Starship
          <$> starshipObj .: "name"
          <*> starshipObj .: "model"
          <*> starshipObj .: "manufacturer"
          <*> starshipObj .: "cost_in_credits"
          <*> starshipObj .: "length"
          <*> starshipObj .: "max_atmosphering_speed"
          <*> starshipObj .: "crew"
          <*> starshipObj .: "passengers"
          <*> starshipObj .: "cargo_capacity"
          <*> starshipObj .: "consumables"
          <*> starshipObj .: "hyperdrive_rating"
          <*> starshipObj .: "MGLT"
          <*> starshipObj .: "starship_class"
          <*> starshipObj .: "pilots"
          <*> starshipObj .: "films"
          <*> starshipObj .: "created"
          <*> starshipObj .: "edited"
          <*> starshipObj .: "url"

instance ToJSON (Starship :: Type) where
  toJSON :: Starship -> Value
  toJSON starship =
    Aeson.object
      [ "name"                   .= sName starship
      , "model"                  .= sModel starship
      , "manufacturer"           .= sManufacturer starship
      , "cost_in_credits"        .= sCost starship
      , "length"                 .= sLength starship
      , "max_atmosphering_speed" .= sMaxAtmospheringSpeed starship
      , "crew"                   .= sRequiredCrew starship
      , "passengers"             .= sPassengerLimit starship
      , "cargo_capacity"         .= sCargoCapacity starship
      , "consumables"            .= sConsumables starship
      , "hyperdrive_rating"      .= sHyperdriveRating starship
      , "MGLT"                   .= sMaxMegalight starship
      , "starship_class"         .= sStarshipClass starship
      , "pilots"                 .= sPilots starship
      , "films"                  .= sFilms starship
      , "created"                .= sCreatedAt starship
      , "edited"                 .= sEditedAt starship
      , "url"                    .= sId starship
      ]

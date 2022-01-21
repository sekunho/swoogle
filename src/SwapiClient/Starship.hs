{-# language FlexibleInstances #-}

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
  , MaxMegalight (MaxMegalight, UnknownMegalight)
  , HyperdriveRating (Rating, UnknownRating)
  , CargoCapacity (Capacity, UnknownCapacity)
  , StarshipLength (StarshipLength)
  , PassengerLimit (PassengerLimit, PLNotApplicable, UnknownPassengerLimit)
  , MaxAtmospheringSpeed (MaxSpeed, MASNotApplicable)
  , CostInCredits (Amount, UnknownCost)
  , Manufacturer (Manufacturer)
  , StarshipModel (StarshipModel)
  , StarshipName (StarshipName)
  , Consumable (CHour, CDay, CWeek, CMonth, CYear, UnknownConsumable)
  , RequiredCrew (CrewRange, CrewAmount, UnknownAmount)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, (.=), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value (String))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (split, toLower, filter)
import Data.Text.Read qualified as Text.Read (decimal, double)
import TextShow (TextShow)
import TextShow qualified as Text.Show (showt)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------

import SwapiClient.Page
  ( Index
    (Index
    , iCount
    , iNextPage
    , iPreviousPage
    , iResults
    )
  )
import SwapiClient.Id (StarshipId, PersonId, FilmId)

--------------------------------------------------------------------------------
-- Data types

data RequiredCrew
  = CrewRange Word Word
  | CrewAmount Word
  | UnknownAmount
  deriving (Eq, Show)

data Consumable
  = CHour Word
  | CDay Word
  | CWeek Word
  | CMonth Word
  | CYear Word
  | UnknownConsumable
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
  | UnknownSpeed
  deriving (Eq, Show)

data PassengerLimit
  = PassengerLimit Word
  | PLNotApplicable
  | UnknownPassengerLimit
  deriving (Eq, Show)

newtype StarshipLength = StarshipLength Double
  deriving stock (Eq, Show)
  deriving newtype TextShow

data CargoCapacity
  = Capacity Word
  | UnknownCapacity
  deriving stock (Eq, Show)

data HyperdriveRating
  = Rating Double
  | UnknownRating
  deriving stock (Eq, Show)

data MaxMegalight
  = MaxMegalight Word
  | UnknownMegalight
  deriving stock (Eq, Show)

data StarshipClass
  = Corvette
  | StarDestroyer
  | LandingCraft
  | DeepSpaceMobileBattlestation
  | LightFreighter
  | Freighter
  | AssaultStarfighter
  | Starfighter
  | StarDreadnought
  | PatrolCraft
  | ArmedGovernmentTransport
  | Transport
  | MediumTransport
  | SpaceTransport
  | EscortShip
  | StarCruiser
  | SpaceCruiser
  | DroidControlShip
  | Yacht
  | DiplomaticBarge
  | AssaultShip
  | CapitalShip
  | Cruiser
  deriving stock (Eq, Show)

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
  , sPilots               :: [PersonId]
  , sFilms                :: [FilmId]
  , sCreatedAt            :: UTCTime
  , sEditedAt             :: UTCTime
  , sId                   :: StarshipId
  }
  deriving stock (Eq, Show)

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
        case Text.Read.double (normalizeNumText lengthText) of
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
        "n/a" -> pure MASNotApplicable

        "unknown" -> pure UnknownSpeed

        iAmSpeed ->
          case Text.Read.decimal (normalizeNumText iAmSpeed) of
            Right (speed, _) ->
              pure (MaxSpeed speed)

            Left e ->
              fail e

instance ToJSON (MaxAtmospheringSpeed :: Type) where
  toJSON :: MaxAtmospheringSpeed -> Value
  toJSON = String .
    \case
      MaxSpeed maxSpeed -> Text.Show.showt maxSpeed <> "km"
      MASNotApplicable -> "n/a"
      UnknownSpeed -> "unknown"

instance FromJSON (RequiredCrew :: Type) where
  parseJSON :: Value -> Parser RequiredCrew
  parseJSON =
    Aeson.withText "RequiredCrew" $
      \case
        "unknown" -> pure UnknownAmount

        val ->
          case parseAmount val of
            Right [minCrew, maxCrew] -> pure (CrewRange minCrew maxCrew)
            Right [crewAmount] -> pure (CrewAmount crewAmount)
            Right _ -> fail "Unexpected format for `crew`"
            Left e -> fail e
    where
      parseAmount :: Text -> Either String [Word]
      parseAmount =
          mapM ((<$>) fst . Text.Read.decimal)
          . Text.split (== '-')
          . normalizeNumText

instance ToJSON (RequiredCrew :: Type) where
  toJSON :: RequiredCrew -> Value
  toJSON = String .
    \case
      CrewRange minCrew maxCrew ->
        Text.Show.showt minCrew <> "-" <> Text.Show.showt maxCrew

      CrewAmount amount -> Text.Show.showt amount
      UnknownAmount -> "unknown"

instance FromJSON (PassengerLimit :: Type) where
  parseJSON :: Value -> Parser PassengerLimit
  parseJSON =
    Aeson.withText "PassengerLimit" $
      \case
        "n/a" -> pure PLNotApplicable

        "unknown" -> pure UnknownPassengerLimit

        limitText ->
          case Text.Read.decimal (normalizeNumText limitText) of
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

      UnknownPassengerLimit ->
        "unknown"

instance FromJSON (CargoCapacity :: Type) where
  parseJSON :: Value -> Parser CargoCapacity
  parseJSON =
    Aeson.withText "CargoCapacity" $
      \case
        "unknown" -> pure UnknownCapacity
        val ->
          case Text.Read.decimal val of
            Right (cargoCapacity, "") ->
              pure (Capacity cargoCapacity)

            Right _ ->
              fail "Unexpected format for `cargo_capacity`."

            Left e ->
              fail e

instance ToJSON (CargoCapacity :: Type) where
  toJSON :: CargoCapacity -> Value
  toJSON = String .
    \case
      UnknownCapacity -> "unknown"
      Capacity capacity -> Text.Show.showt capacity

instance FromJSON (Consumable :: Type) where
  parseJSON :: Value -> Parser Consumable
  parseJSON =
    Aeson.withText "Consumable" $
      \case
        "unknown" -> pure UnknownConsumable
        val ->
          case Text.Read.decimal val of
            Right (timeLength, " hour") ->
              pure (CHour timeLength)

            Right (timeLength, " hours") ->
              pure (CHour timeLength)

            Right (timeLength, " day") ->
              pure (CDay timeLength)

            Right (timeLength, " days") ->
              pure (CDay timeLength)

            Right (timeLength, " week") ->
              pure (CWeek timeLength)

            Right (timeLength, " weeks") ->
              pure (CWeek timeLength)

            Right (timeLength, " month") ->
              pure (CMonth timeLength)

            Right (timeLength, " months") ->
              pure (CMonth timeLength)

            Right (timeLength, " year") ->
              pure (CYear timeLength)

            Right (timeLength, " years") ->
              pure (CYear timeLength)

            Right _ ->
              fail "Unexpected format for `consumables`"

            Left e ->
              fail e

instance ToJSON (Consumable :: Type) where
  toJSON :: Consumable -> Value
  toJSON = String .
    \case
      CHour timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " hour"

      CDay timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " day"

      CWeek timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " week"

      CMonth timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " month"

      CYear timeLength ->
        Text.Show.showt timeLength <> appendS timeLength " year"

      UnknownConsumable ->
        "unknown"

    where
      appendS :: Word -> Text -> Text
      appendS timeLength txt
        | timeLength > 1 = txt <> "s"
        | otherwise = txt

instance FromJSON (HyperdriveRating :: Type) where
  parseJSON :: Value -> Parser HyperdriveRating
  parseJSON =
    Aeson.withText "HyperdriveRating" $
      \case
        "unknown" -> pure UnknownRating
        val ->
          case Text.Read.double val of
            Right (rating, "") ->
              pure (Rating rating)

            Right _ ->
              fail "Unexpected format for `hyperdrive_rating`"

            Left e ->
              fail e

instance ToJSON (HyperdriveRating :: Type) where
  toJSON :: HyperdriveRating -> Value
  toJSON = String .
    \case
      UnknownRating -> "unknown"
      Rating rating -> Text.Show.showt rating

instance FromJSON (MaxMegalight :: Type) where
  parseJSON :: Value -> Parser MaxMegalight
  parseJSON =
    Aeson.withText "MaxMegalight" $
      \case
        "unknown" -> pure UnknownMegalight
        val ->
          case Text.Read.decimal val of
            Right (maxMegalight, "") ->
              pure (MaxMegalight maxMegalight)

            Right _ ->
              fail "Unexpected format for `MGLT`"

            Left e ->
              fail e

instance ToJSON (MaxMegalight :: Type) where
  toJSON :: MaxMegalight -> Value
  toJSON = String .
    \case
      UnknownMegalight -> "unknown"
      MaxMegalight megalight -> Text.Show.showt megalight

instance FromJSON (StarshipClass :: Type) where
  parseJSON :: Value -> Parser StarshipClass
  parseJSON =
    Aeson.withText "StarshipClass" $
      \val ->
        case Text.toLower val of
          "corvette" -> pure Corvette
          "star destroyer" -> pure StarDestroyer
          "landing craft" -> pure LandingCraft
          "deep space mobile battlestation" -> pure DeepSpaceMobileBattlestation
          "light freighter" -> pure LightFreighter
          "assault starfighter" -> pure AssaultStarfighter
          "starfighter" -> pure Starfighter
          "star dreadnought" -> pure StarDreadnought
          "medium transport" -> pure MediumTransport
          "patrol craft" -> pure PatrolCraft
          "armed government transport" -> pure ArmedGovernmentTransport
          "escort ship" -> pure EscortShip
          "star cruiser" -> pure StarCruiser
          "space cruiser" -> pure SpaceCruiser
          "droid control ship" -> pure DroidControlShip
          "yacht" -> pure Yacht
          "space transport" -> pure SpaceTransport
          "diplomatic barge" -> pure DiplomaticBarge
          "freighter" -> pure Freighter
          "assault ship" -> pure AssaultShip
          "capital ship" -> pure CapitalShip
          "transport" -> pure Transport
          "cruiser" -> pure Cruiser
          _ -> fail "Unexpected value for `starship_class`."

instance ToJSON (StarshipClass :: Type) where
  toJSON :: StarshipClass -> Value
  toJSON = String .
    \case
      Corvette -> "Corvette"
      StarDestroyer -> "Star Destroyer"
      LandingCraft -> "Landing Craft"
      DeepSpaceMobileBattlestation -> "Deep Space Mobile Battlestation"
      LightFreighter -> "Light Freighter"
      AssaultStarfighter -> "Assault Starfighter"
      Starfighter -> "Starfighter"
      StarDreadnought -> "Star Dreadnought"
      MediumTransport -> "Medium Transport"
      Transport -> "Transport"
      SpaceTransport -> "Space Transport"
      EscortShip -> "Escort Ship"
      StarCruiser -> "Star Cruiser"
      SpaceCruiser -> "Space Cruiser"
      DroidControlShip -> "Droid Control Ship"
      Yacht -> "Yacht"
      DiplomaticBarge -> "Diplomatic Barge"
      AssaultShip -> "Assault Ship"
      CapitalShip -> "Capital Ship"
      Cruiser -> "Cruiser"
      Freighter -> "Freighter"
      ArmedGovernmentTransport -> "Armed Government Transport"
      PatrolCraft -> "Patrol Craft"

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

instance FromJSON (Index Starship :: Type) where
  parseJSON :: Value -> Parser (Index Starship)
  parseJSON =
    Aeson.withObject "Index Starship" $
      \val ->
        Index
          <$> val .: "count"
          <*> val .: "next"
          <*> val .: "previous"
          <*> val .: "results"

instance ToJSON (Index Starship :: Type) where
  toJSON :: Index Starship -> Value
  toJSON starshipIndex =
    Aeson.object
      [ "count"    .= iCount starshipIndex
      , "next"     .= iNextPage starshipIndex
      , "previous" .= iPreviousPage starshipIndex
      , "results"  .= iResults starshipIndex
      ]

--------------------------------------------------------------------------------
-- Utils

normalizeNumText :: Text -> Text
normalizeNumText = Text.filter (/= ',')

module SwapiClient.Starship
  ( Starship ( Starship
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
    ( SCCorvette
    , SCStarDestroyer
    , SCLandingCraft
    , SCDeepSpaceMobileBattlestation
    , SCLightFreighter
    , SCAssaultStarfighter
    , SCStarfighter
    , SCStarDreadnought
    , SCMediumTransport
    )
  , MaxMegalight (MaxMegalight, UnknownMegalight)
  , HyperdriveRating (Rating, UnknownRating)
  , CargoCapacity (Capacity, UnknownCapacity)
  , StarshipLength (StarshipLength)
  , PassengerLimit (PassengerLimit, PLNotApplicable, UnknownPassengerLimit)
  , MaxAtmospheringSpeed (MaxSpeed, MASNotApplicable)
  , Cost (Credits, UnknownCost)
  , Manufacturer (Manufacturer)
  , StarshipModel (StarshipModel)
  , StarshipName (StarshipName)
  , Consumable (CHour, CDay, CWeek, CMonth, CYear, UnknownConsumable)
  , RequiredCrew (CrewRange, CrewAmount, UnknownAmount)
  , Wrapped (Wrapped)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson       (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Aeson       qualified as Aeson
import Data.Aeson.Types (Parser, Value (String))
import Data.Coerce      (Coercible, coerce)
-- import Data.Coerce qualified as Coerce (coerce)
import Data.Kind        (Type)
import Data.Text        (Text)
import Data.Text        qualified as Text (filter, split, toLower)
import Data.Text.Read   qualified as Text.Read (decimal, double)
import Data.Time        (UTCTime)
import TextShow         (TextShow)
import TextShow         qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id   (FilmId, PersonId, StarshipId)
import SwapiClient.Page (Index (Index, iCount, iNextPage, iPreviousPage, iResults))

--------------------------------------------------------------------------------
-- Data types

newtype Wrapped a = Wrapped a
  deriving stock (Eq, Show)

data RequiredCrew
  = CrewRange (Word, Word)
  | CrewAmount Word
  | UnknownAmount
  deriving stock (Eq, Show)

data Consumable
  = CHour Word
  | CDay Word
  | CWeek Word
  | CMonth Word
  | CYear Word
  | LiveFoodTanks
  | NoConsumable
  | UnknownConsumable
  deriving stock (Eq, Show)

newtype StarshipName = StarshipName Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

newtype StarshipModel = StarshipModel Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

newtype Manufacturer = Manufacturer Text
  deriving stock (Eq, Show)
  deriving ToJSON via (Wrapped Text)

data Cost
  = Credits Word
  | UnknownCost
  deriving stock (Eq, Show)

data MaxAtmospheringSpeed
  = MaxSpeed Word
  | MASNotApplicable
  | UnknownSpeed
  deriving stock (Eq, Show)

data PassengerLimit
  = PassengerLimit Word
  | PLNotApplicable
  | UnknownPassengerLimit
  deriving stock (Eq, Show)

newtype StarshipLength = StarshipLength Double
  deriving stock (Eq, Show)
  deriving newtype TextShow
  deriving ToJSON via (Wrapped Double)

data CargoCapacity
  = Capacity Word
  | NoCapacity
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
  = SCCorvette
  | SCStarDestroyer
  | SCLandingCraft
  | SCDeepSpaceMobileBattlestation
  | SCLightFreighter
  | SCFreighter
  | SCAssaultStarfighter
  | SCStarfighter
  | SCStarDreadnought
  | SCPatrolCraft
  | SCArmedGovernmentTransport
  | SCTransport
  | SCMediumTransport
  | SCSpaceTransport
  | SCEscortShip
  | SCStarCruiser
  | SCSpaceCruiser
  | SCDroidControlShip
  | SCYacht
  | SCDiplomaticBarge
  | SCAssaultShip
  | SCCapitalShip
  | SCCruiser
  deriving stock (Eq, Show)

data Starship = Starship
  { sName                 :: StarshipName
  , sModel                :: StarshipModel
  , sManufacturer         :: Manufacturer
  , sCost                 :: Cost
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

-- instance ToJSON (StarshipName :: Type) where
--   toJSON :: StarshipName -> Value
--   toJSON = String . Text.Show.showt

instance FromJSON (Cost :: Type) where
  parseJSON :: Value -> Parser Cost
  parseJSON = Aeson.withText "Cost" $
    \case
      "unknown" ->
        pure UnknownCost

      costText ->
        -- TODO: I unfortunately do not know a better way to parse `Text`.
        case Text.Read.decimal costText of
          Right (amount, "") ->
            pure $ Credits amount

          Right _ ->
            fail "Unexpected format for cost"

          Left e ->
            fail e

instance ToJSON (Cost :: Type) where
  toJSON :: Cost -> Value
  toJSON = String .
    \case
      Credits amount -> Text.Show.showt amount
      UnknownCost    -> "unknown"

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
      MaxSpeed maxSpeed -> Text.Show.showt maxSpeed
      MASNotApplicable  -> "n/a"
      UnknownSpeed      -> "unknown"

instance FromJSON (RequiredCrew :: Type) where
  parseJSON :: Value -> Parser RequiredCrew
  parseJSON =
    Aeson.withText "RequiredCrew" $
      \case
        "unknown" -> pure UnknownAmount

        val ->
          case parseAmount val of
            Right [minCrew, maxCrew] -> pure (CrewRange (minCrew, maxCrew))
            Right [crewAmount]       -> pure (CrewAmount crewAmount)
            Right _                  -> fail "Unexpected format for `crew`"
            Left e                   -> fail e
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
      CrewRange (minCrew, maxCrew) ->
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
        "none" -> pure NoCapacity
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
      Capacity capacity -> Text.Show.showt capacity
      UnknownCapacity   -> "unknown"
      NoCapacity        -> "none"

instance FromJSON (Consumable :: Type) where
  parseJSON :: Value -> Parser Consumable
  parseJSON =
    Aeson.withText "Consumable" $
      \case
        "unknown" -> pure UnknownConsumable
        "none" -> pure NoConsumable
        "Live food tanks" -> pure LiveFoodTanks
        val ->
          case Text.Read.decimal val of
            Right (0, "") ->
              pure NoConsumable

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

      LiveFoodTanks -> "Live food tanks"

      NoConsumable -> "none"
      UnknownConsumable -> "unknown"

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
      UnknownMegalight       -> "unknown"
      MaxMegalight megalight -> Text.Show.showt megalight

instance FromJSON (StarshipClass :: Type) where
  parseJSON :: Value -> Parser StarshipClass
  parseJSON =
    Aeson.withText "StarshipClass" $
      \val ->
        case Text.toLower val of
          "corvette" -> pure SCCorvette
          "star destroyer" -> pure SCStarDestroyer
          "landing craft" -> pure SCLandingCraft
          "deep space mobile battlestation" -> pure SCDeepSpaceMobileBattlestation
          "light freighter" -> pure SCLightFreighter
          "assault starfighter" -> pure SCAssaultStarfighter
          "starfighter" -> pure SCStarfighter
          "star dreadnought" -> pure SCStarDreadnought
          "medium transport" -> pure SCMediumTransport
          "patrol craft" -> pure SCPatrolCraft
          "armed government transport" -> pure SCArmedGovernmentTransport
          "escort ship" -> pure SCEscortShip
          "star cruiser" -> pure SCStarCruiser
          "space cruiser" -> pure SCSpaceCruiser
          "droid control ship" -> pure SCDroidControlShip
          "yacht" -> pure SCYacht
          "space transport" -> pure SCSpaceTransport
          "diplomatic barge" -> pure SCDiplomaticBarge
          "freighter" -> pure SCFreighter
          "assault ship" -> pure SCAssaultShip
          "capital ship" -> pure SCCapitalShip
          "transport" -> pure SCTransport
          "cruiser" -> pure SCCruiser
          _ -> fail "Unexpected value for `starship_class`."

instance ToJSON (StarshipClass :: Type) where
  toJSON :: StarshipClass -> Value
  toJSON = String .
    \case
      SCCorvette                     -> "Corvette"
      SCStarDestroyer                -> "Star Destroyer"
      SCLandingCraft                 -> "Landing Craft"
      SCDeepSpaceMobileBattlestation -> "Deep Space Mobile Battlestation"
      SCLightFreighter               -> "Light Freighter"
      SCAssaultStarfighter           -> "Assault Starfighter"
      SCStarfighter                  -> "Starfighter"
      SCStarDreadnought              -> "Star Dreadnought"
      SCMediumTransport              -> "Medium Transport"
      SCTransport                    -> "Transport"
      SCSpaceTransport               -> "Space Transport"
      SCEscortShip                   -> "Escort Ship"
      SCStarCruiser                  -> "Star Cruiser"
      SCSpaceCruiser                 -> "Space Cruiser"
      SCDroidControlShip             -> "Droid Control Ship"
      SCYacht                        -> "Yacht"
      SCDiplomaticBarge              -> "Diplomatic Barge"
      SCAssaultShip                  -> "Assault Ship"
      SCCapitalShip                  -> "Capital Ship"
      SCCruiser                      -> "Cruiser"
      SCFreighter                    -> "Freighter"
      SCArmedGovernmentTransport     -> "Armed Government Transport"
      SCPatrolCraft                  -> "Patrol Craft"

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

instance (Coercible (Wrapped a) a, TextShow a) => ToJSON ((Wrapped a) :: Type) where
  toJSON :: Wrapped a -> Value
  toJSON = String . Text.Show.showt . coerce @(Wrapped a) @a

instance FromJSON Manufacturer where
  parseJSON :: Value -> Parser Manufacturer
  parseJSON = Aeson.withText "Manufacturer" (pure . Manufacturer)

instance FromJSON StarshipModel where
  parseJSON :: Value -> Parser StarshipModel
  parseJSON = Aeson.withText "StarshipModel" (pure . StarshipModel)

--------------------------------------------------------------------------------
-- Utils

normalizeNumText :: Text -> Text
normalizeNumText = Text.filter (/= ',')

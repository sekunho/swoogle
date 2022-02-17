{- |
This module contains:

1. Data types for a species
2. `aeson` `FromJSON` instances for decoding JSON objects

__Decoding species JSON objects__

Let's say you have this JSON response from
@https://swapi.dev/api/species/1/?format=json@:

> {
>   "name": "Human",
>   "classification": "mammal",
>   "designation": "sentient",
>   "average_height": "180",
>   "skin_colors": "caucasian, black, asian, hispanic",
>   "hair_colors": "blonde, brown, black, red",
>   "eye_colors": "brown, blue, green, hazel, grey, amber",
>   "average_lifespan": "120",
>   "homeworld": "https://swapi.dev/api/planets/9/",
>   "language": "Galactic Basic",
>   "people": [
>     "https://swapi.dev/api/people/66/",
>     "https://swapi.dev/api/people/67/",
>     "https://swapi.dev/api/people/68/",
>     "https://swapi.dev/api/people/74/"
>   ],
>   "films": [
>     "https://swapi.dev/api/films/1/",
>     "https://swapi.dev/api/films/2/",
>     "https://swapi.dev/api/films/3/",
>     "https://swapi.dev/api/films/4/",
>     "https://swapi.dev/api/films/5/",
>     "https://swapi.dev/api/films/6/"
>   ],
>   "created": "2014-12-10T13:52:11.567000Z",
>   "edited": "2014-12-20T21:36:42.136000Z",
>   "url": "https://swapi.dev/api/species/1/"
> }

And bind it to `sampleJSON`. Let's try decoding it!

@
import Data.Aeson
import Swapi.Resource.Species

-- I'm assuming `sampleJSON` is a ByteString
someSpecies :: Maybe SpeciesType
someSpecies = decode sampleJSON
@

@someSpecies@ would be

@
Just $
  HasOrigin $
    MkSpecies
      { spName = SpeciesName "Human"
      , spClassification = MammalClass
      , spDesignation = SentientDesignation
      , spAverageHeight = Height 180
      , spSkinColors = [CaucasianSkin,BlackSkin,AsianSkin,HispanicSkin]
      , spHairColors = [BlondHair,BrownHair,BlackHair,RedHair]
      , spEyeColors = [BrownEye,BlueEye,GreenEye,HazelEye,GreyEye,AmberEye]
      , spAverageLifespan = Lifespan 120
      , spHomeworld = PlanetId 9
      , spLanguage = GalacticBasic
      , spPeople = [PersonId 66,PersonId 67,PersonId 68,PersonId 74]
      , spFilms = [FilmId 1,FilmId 2,FilmId 3,FilmId 4,FilmId 5,FilmId 6]
      , spCreatedAt = 2014-12-10 13:52:11.567 UTC
      , spEditedAt = 2014-12-20 21:36:42.136 UTC
      , spId = SpeciesId 1
      }
@

1. Some values were further parsed into more specific types. Like `HairColor`,
   `SkinColor`, etc.
2. The JSON object's `url` field was parsed into a `SpeciesId`, since we won't
   deal with the URL itself to communicate with the API. Less error-prone this
   way. See `Swapi.Api` on how to communicate with @https://swapi.dev@.
3. Similar to the previous point, all IDs were parsed into their own ID types.
-}
module Swapi.Resource.Species
  ( SpeciesName (SpeciesName)
  , Designation (SentientDesignation, ReptilianDesignation)
  , Classification
    ( MammalClass
    , ArtificialClass
    , SentientClass
    , GastropodClass
    , ReptileClass
    , AmphibianClass
    )
  , AverageHeight (AverageHeight, AverageHeightNotApplicable)
  , AverageLifespan (Lifespan, Indefinite, UnknownLifespan)
  , Language
    ( GalacticBasic
    , Shyriiwook
    , Huttese
    , Dosh
    , MonCalamarian
    , Ewokese
    , Sullutese
    , NoLanguage
    )
  , SpeciesType (HasOrigin, NoOrigin)
  , Species
    ( MkSpecies
    , spName
    , spClassification
    , spDesignation
    , spAverageHeight
    , spSkinColors
    , spHairColors
    , spEyeColors
    , spAverageLifespan
    , spHomeworld
    , spLanguage
    , spPeople
    , spFilms
    , spCreatedAt
    , spEditedAt
    , spId
    )
  , OriginlessSpecies
    ( MkOriginlessSpecies
    , hSpName
    , hSpClassification
    , hSpDesignation
    , hSpAverageHeight
    , hSpSkinColors
    , hSpHairColors
    , hSpEyeColors
    , hSpAverageLifespan
    , hSpLanguage
    , hSpPeople
    , hSpFilms
    , hSpCreatedAt
    , hSpEditedAt
    , hSpId
    )
  ) where

--------------------------------------------------------------------------------

import Data.Aeson                ((.:))
import Data.Aeson                qualified as Aeson (withObject, withText)
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.Types          (FromJSON, Parser,
                                  Value (Null, Object, String), parseJSON)
import Data.Kind                 (Type)
import Data.Text                 (Text)
import Data.Text                 qualified as Text (toLower, unpack)
import Data.Text.Read            qualified as Text.Read (decimal)
import Data.Time                 (UTCTime)

--------------------------------------------------------------------------------

import Swapi.Color         (EyeColor, HairColor, SkinColor)
import Swapi.Id            (FilmId, PersonId, PlanetId, SpeciesId)
import Swapi.Internal.Page (Index (Index))

--------------------------------------------------------------------------------
-- Data types

-- | A species' name
newtype SpeciesName = SpeciesName Text
  -- ^ A wrapper to prevent mixing up a `SpeciesName` with any `Text`.
  deriving stock
    ( Eq   -- ^ Compare `SpeciesName`s with each other
    , Show -- ^ Encode `SpeciesName` as `String` through `show`
    )

-- | Designation of any species
data Designation
  = SentientDesignation   -- ^ What does it mean to be alive?
  | ReptilianDesignation  -- ^ Mark Zuckerberg lol
  deriving stock
    ( Eq   -- ^ Compare `Designation`s with each other
    , Show -- ^ Encode `Designation` as `String` through `show`
    )

-- | A species' classification
data Classification
  = MammalClass
  | ArtificialClass
  | SentientClass
  | GastropodClass
  | ReptileClass
  | AmphibianClass
  deriving stock
    ( Eq   -- ^ Compare `Classification`s with each other
    , Show -- ^ Encode `Classification` as `String` through `show`
    )

-- | Average height of a species in centimeters
data AverageHeight
  = AverageHeight Word         -- ^ A species' average height in centimeters (cm)
  | AverageHeightNotApplicable -- ^ This dude says they're 6'5" and won't admit any less
  deriving stock
    ( Eq   -- ^ Compare `AverageHeight`s with each other
    , Show -- ^ Encode `AverageHeight` as `String` through `show`
    )

-- | Average lifespan of a species in years
data AverageLifespan
  = Lifespan Word    -- ^ A species' average lifespan in years
  | Indefinite       -- ^ Immortal. Must be boring.
  | UnknownLifespan  -- ^ Don't know for how long this one lives
  deriving stock
    ( Eq   -- ^ Compare `AverageLifespan`s with each other
    , Show -- ^ Encode `AverageLifespan` as `String` through `show`
    )

-- | Language used by a species
data Language
  = GalacticBasic -- ^ The basic b
  | Shyriiwook    -- ^ __Uuuuuuraaagghh Ahhhhhhhr!__
  | Huttese       -- ^ The dummy thicc language
  | Dosh          -- ^ Short for Trandoshan; official language of the Trandoshan species
  | MonCalamarian -- ^ Official language of the Mon Calamari species. /I'm hungry.../
  | Ewokese       -- ^ Fluffy teddy bears that have spears
  | Sullutese     -- ^ Official language of the Sullustan species
  | NoLanguage    -- ^ They don't communicate. Perhaps the archives have been deleted?
  deriving stock
    ( Eq   -- ^ Compare `Language`s with each other
    , Show -- ^ Encode `Language` as `String` through `show`
    )

{- |
Distinguishes between the two types of species:

1. One with origin (has a homeworld)
2. One without (doesn't have a homeworld)

The reason behind why I split `Species` is here:
@https://github.com/sekunho/swapi/pull/24@
-}
data SpeciesType
  = HasOrigin Species
  | NoOrigin OriginlessSpecies
  deriving stock
    ( Eq   -- ^ Compare `SpeciesType`s with each other
    , Show -- ^ Encode `SpeciesType` as `String` through `show`
    )

-- | A representation of a species with origin.
data Species = MkSpecies
  { spName            :: SpeciesName      -- ^ Name of this species
  , spClassification  :: Classification   -- ^ Species classification
  , spDesignation     :: Designation      -- ^ Designation of this species
  , spAverageHeight   :: AverageHeight    -- ^ Average height
  , spSkinColors      :: [SkinColor]      -- ^ Skin color combinations of this species
  , spHairColors      :: [HairColor]      -- ^ Hair combinations of this species
  , spEyeColors       :: [EyeColor]       -- ^ Eye color of this species
  , spAverageLifespan :: AverageLifespan  -- ^ Average lifespan
  , spHomeworld       :: PlanetId         -- ^ Species' origin
  , spLanguage        :: Language         -- ^ Language used by this species
  , spPeople          :: [PersonId]       -- ^ People who are a part of this species
  , spFilms           :: [FilmId]         -- ^ Film appearances
  , spCreatedAt       :: UTCTime          -- ^ When this entry was created (API)
  , spEditedAt        :: UTCTime          -- ^ When this entry was last edited (API)
  , spId              :: SpeciesId        -- ^ Species ID in the API's database
  }
  deriving stock
    ( Eq   -- ^ Compare `AverageHeight`s with each other
    , Show -- ^ Encode `AverageHeight` as `String` through `show`
    )

-- | A representation of a species without an origin (without a homeworld).
data OriginlessSpecies = MkOriginlessSpecies
  { hSpName            :: SpeciesName      -- ^ Name of this originless species
  , hSpClassification  :: Classification   -- ^ Originless species classification
  , hSpDesignation     :: Designation      -- ^ Designation of this originless species
  , hSpAverageHeight   :: AverageHeight    -- ^ Average height
  , hSpSkinColors      :: [SkinColor]      -- ^ Skin color combinations of this originless species
  , hSpHairColors      :: [HairColor]      -- ^ Hair color combinations of this originless species
  , hSpEyeColors       :: [EyeColor]       -- ^ Eye color combinations of this originless species
  , hSpAverageLifespan :: AverageLifespan  -- ^ Average lifespan
  , hSpLanguage        :: Language         -- ^ Language used by this originless species
  , hSpPeople          :: [PersonId]       -- ^ People who are a part of this originless species
  , hSpFilms           :: [FilmId]         -- ^ Film appearances
  , hSpCreatedAt       :: UTCTime          -- ^ When this entry was created (API)
  , hSpEditedAt        :: UTCTime          -- ^ When this entry was last edited (API)
  , hSpId              :: SpeciesId        -- ^ Species ID in the API's database
  }
  deriving stock
    ( Eq   -- ^ Compare `AverageHeight`s with each other
    , Show -- ^ Encode `AverageHeight` as `String` through `show`
    )

--------------------------------------------------------------------------------
-- Instances

{- |
Decode a string JSON value to a `SpeciesName`.

Any string is fine.
-}
instance FromJSON (SpeciesName :: Type) where
  parseJSON :: Value -> Parser SpeciesName
  parseJSON = Aeson.withText "SpeciesName" (pure . SpeciesName)

{- |
Decode a string JSON value to a `Designation`.

A JSON value can be decoded to `Designation` but must be one of these formats:

1. @"sentient"@
2. @"reptilian"@

Case sensitive
-}
instance FromJSON (Designation :: Type) where
  parseJSON :: Value -> Parser Designation
  parseJSON =
    Aeson.withText "Designation" $
      \case
        "sentient"  -> pure SentientDesignation
        "reptilian" -> pure ReptilianDesignation
        _           -> fail "Unexpected value for species' designation"

{- |
Decode a string JSON value to a `Classification`.

A JSON value can be decoded to `Classification` but must be one of these formats:

1. @"mammal"@
2. @"artificial"@
3. @"sentient"@
4. @"gastropod"@
5. @"reptile"@
6. @"amphibian"@

Case sensitive
-}
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

{- |
Decode a string JSON value to a `AverageHeight`.

A JSON value can be decoded to `AverageHeight` but must be one of these formats:

1. @"n/a"@
2. Any integer literal encoded as a string

__Examples for #2__

__Good__

@
"123"
"69"
"42"
@

__Bad__

@
"1,234"
"1234.00"
"123m"
"12 "
" 19"
"1 5"
"i swear i'm 6 ft 5 inches!"
@

Case sensitive
-}
instance FromJSON (AverageHeight :: Type) where
  parseJSON :: Value -> Parser AverageHeight
  parseJSON =
    Aeson.withText "AverageHeight" $
      \case
        "n/a" -> pure AverageHeightNotApplicable
        val ->
          case Text.Read.decimal val of
            Right (avgHeight, "") ->
              pure (AverageHeight avgHeight)

            Right _ -> fail "Unexpected value for species' average height"
            Left e -> fail e

{- |
Decode a string JSON value to a `AverageLifespan`.

A JSON value can be decoded to `AverageLifespan` but must be one of these formats:

1. @"indefinite"@
2. @"unknown"@
3. Any integer literal encoded as a string

__Examples for #3__

__Good__

@
"123"
"69"
"42"
@

__Bad__

@
"1,234"
"1234.00"
"123 years"
"12 "
" 19"
"1 5"
"pushing 80"
@

Case sensitive
-}
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

{- |
Decode a string JSON value to a `Language`.

A JSON value can be decoded to `Language` but must be one of these formats:

1. @"galactic basic"@
2. @"galatic basic"@ /Yeah, I know, this is a typo on swapi's end./
3. @"shryiiwook"@
4. @"huttese"@
5. @"dosh"@
6. @"mon calamarian"@
7. @"ewokese"@
8. @"sullutese"@
9. @"n/a"@

Not case sensitive
-}
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

-- | Decodes a JSON object to a `Species` (with origin).
instance FromJSON (Species :: Type) where
  parseJSON :: Value -> Parser Species
  parseJSON =
    Aeson.withObject "Species" $
      \val ->
        MkSpecies
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

-- | Decodes a JSON object to an `OriginlessSpecies`.
instance FromJSON (OriginlessSpecies :: Type) where
  parseJSON :: Value -> Parser OriginlessSpecies
  parseJSON =
    Aeson.withObject "OriginlessSpecies" $
      \val ->
        MkOriginlessSpecies
          <$> val .: "name"
          <*> val .: "classification"
          <*> val .: "designation"
          <*> val .: "average_height"
          <*> val .: "skin_colors"
          <*> val .: "hair_colors"
          <*> val .: "eye_colors"
          <*> val .: "average_lifespan"
          <*> val .: "language"
          <*> val .: "people"
          <*> val .: "films"
          <*> val .: "created"
          <*> val .: "edited"
          <*> val .: "url"

-- | Decodes a JSON object to a `SpeciesType`.
instance FromJSON (SpeciesType :: Type) where
  parseJSON :: Value -> Parser SpeciesType
  parseJSON =
    Aeson.withObject "SpeciesType" $
      \val ->
        case HashMap.lookup "homeworld" val of
          Nothing -> fail "Species is supposed to contain a homeworld field."
          Just val' -> case val' of
            String _ -> HasOrigin <$> parseJSON @Species (Object val)
            Null -> NoOrigin <$> parseJSON @OriginlessSpecies (Object val)
            _ ->
              fail "Species' homeworld field is supposed to be null or a string."

-- | Decodes an index of species to an `Index SpeciesType`.
instance FromJSON (Index SpeciesType :: Type) where
  parseJSON :: Value -> Parser (Index SpeciesType)
  parseJSON =
    Aeson.withObject "Index Species" $
      \val ->
        Index
          <$> val .: "count"
          <*> val .: "next"
          <*> val .: "previous"
          <*> val .: "results"

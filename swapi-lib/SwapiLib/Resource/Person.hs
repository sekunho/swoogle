{- |
This module contains:

1. Data types for a person
2. `aeson` instances for dealing with person JSON objects

You can decode/encode any data type in this module from and to JSON values.

__Decoding person JSON objects__

Let's say you have this JSON response from
@https://swapi.dev/api/people/1/?format=json@:

@
{ "name":"Luke Skywalker"
, "height":"172"
, "mass":"77"
, "hair_color":"blond"
, "skin_color":"fair"
, "eye_color":"blue"
, "birth_year":"19BBY"
, "gender":"male"
, "homeworld":"https://swapi.dev/api/planets/1/"
, "films":
    [ "https://swapi.dev/api/films/1/"
    , "https://swapi.dev/api/films/2/"
    , "https://swapi.dev/api/films/3/"
    , "https://swapi.dev/api/films/6/"
    ]
, "species":[]
, "vehicles":
    [ "https://swapi.dev/api/vehicles/14/"
    , "https://swapi.dev/api/vehicles/30/"
    ]
, "starships":
  [ "https://swapi.dev/api/starships/12/"
  ,"https://swapi.dev/api/starships/22/"
  ]
, "created":"2014-12-09T13:50:51.644000Z"
, "edited":"2014-12-20T21:17:56.891000Z"
, "url":"https://swapi.dev/api/people/1/"
}
@

And bind it to `sampleJSON`. Let's try decoding it!

>>> import Data.Aeson
>>> import SwapiLib.Resource.Person
>>> decode @Person sampleJSON
Person
{ pName = PersonName "Luke Skywalker"
, pHeight = Height 172
, pMass = Mass 77.0
, pHairColor = [BlondHair]
, pSkinColor = [FairSkin]
, pEyeColor = [BlueEye]
, pBirthYear = BBY 19.0
, pGender = Male
, pHomeworldId = PlanetId 1
, pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 6]
, pSpeciesIds = []
, pVehicleIds = [VehicleId 14,VehicleId 30]
, pStarshipIds = [StarshipId 12,StarshipId 22]
, pCreatedAt = 2014-12-09 13:50:51.644 UTC
, pEditedAt = 2014-12-20 21:17:56.891 UTC
, pId = PersonId 1
}

1. Some values were further parsed into more specific types. Like `HairColor`,
   `SkinColor`, etc.
2. The JSON object's `url` field was parsed into a `PersonId`, since we won't
   deal with the URL itself to communicate with the API. Less error-prone this
   way. See `SwapiLib.Api` on how to communicate with @https://swapi.dev@.
3. Similar to the previous point, all IDs were parsed into their own ID types.

__Encoding `Person` to a JSON object__

You could also do the other way around, say you want to encode a `Person` as a
JSON object. Using the above `Person` record, let's assume we bound it to
`personA`.

>>> import Data.Aeson
>>> import SwapiLib.Resource.Person
>>> encode personA
{
  "birth_year": "19.0BBY",
  "created": "2014-12-09T13:50:51.644Z",
  "edited": "2014-12-20T21:17:56.891Z",
  "eye_color": [
    "blue"
  ],
  "films": [
    "https://swapi.dev/api/films/1/",
    "https://swapi.dev/api/films/2/",
    "https://swapi.dev/api/films/3/",
    "https://swapi.dev/api/films/6/"
  ],
  "gender": "male",
  "hair_color": "blond",
  "height": "172",
  "homeworld": "https://swapi.dev/api/planets/1/",
  "mass": "77.0",
  "name": "Luke Skywalker",
  "skin_color": "fair",
  "species": [],
  "starships": [
    "https://swapi.dev/api/starships/12/",
    "https://swapi.dev/api/starships/22/"
  ],
  "url": "https://swapi.dev/api/people/1/",
  "vehicles": [
    "https://swapi.dev/api/vehicles/14/",
    "https://swapi.dev/api/vehicles/30/"
  ]
}

The result from this is of course filled with @\@ to escape a lot of symbols. I
removed it to make it more readable, but do keep this in mind.
-}
module SwapiLib.Resource.Person
  ( BirthYear (BBY, ABY, UnknownBirthYear)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , Gender (Male, Female, Hermaphrodite, NoGender)
  , PersonName (PersonName)
  , Person
      ( Person
      , pName
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

import Data.Aeson                qualified as Aeson (object, withObject,
                                                     withText)
import Data.Aeson.Types          (FromJSON (parseJSON), KeyValue ((.=)), Parser,
                                  ToJSON (toJSON), Value (String), (.:))

import Data.Kind                 (Type)
import Data.Text                 (Text)
import Data.Text                 qualified as Text (filter, pack)
import Data.Text.Read            qualified as Text.Read (decimal, double)
import Data.Time                 (UTCTime)

--------------------------------------------------------------------------------

import SwapiLib.Color         (EyeColor, HairColor, SkinColor)

import SwapiLib.Id            (FilmId, PersonId, PlanetId, SpeciesId,
                                  StarshipId, VehicleId)

import SwapiLib.Internal.Page (Index (Index, iCount, iNextPage, iPreviousPage, iResults))

--------------------------------------------------------------------------------
-- Data types

{- |
There are two categories of `BirthYear`s:

1. BBY (Before Battle of Yavin); and
2. ABY (After Battle of Yavin)

`Double` represents how long before and after, respectively.

Battle of Yavin: https://starwars.fandom.com/wiki/Battle_of_Yavin
-}
data BirthYear
  = BBY Double         -- ^ How long before the Battle of Yavin
  | ABY Double         -- ^ How long after the Battle of Yavin
  | UnknownBirthYear   -- ^ Birth year is not known or discoverd
  deriving stock
    ( Eq   -- ^ Compare `BirthYear`s with each other
    , Show -- ^ Encode `BirthYear` as `String` through `show`
    )

-- | Height of something
data Height
  = Height Int    -- ^ Height of something in meters
  | UnknownHeight -- ^ Height is not known or discovered
  deriving stock
    ( Eq   -- ^ Compare `Height`s with each other
    , Show -- ^ Encode `Height` as `String` through `show`
    )

-- | Mass of something
data Mass
  = Mass Double -- ^ Mass of something in kilograms
  | UnknownMass -- ^ Mass is not known or discovered
  deriving stock
    ( Eq   -- ^ Compare `Gender`s with each other
    , Show -- ^ Encode `Mass` as a `String` through `show`
    )

-- TODO: Deal with "none"/"unknown"/"n/a" rather than lump it in `NoGender`
-- | Genders in the Star Wars universe
data Gender
  = Male
  | Female
  | Hermaphrodite
  | NoGender      -- ^ Does not have a gender
  deriving stock
    ( Eq   -- ^ Compare `Gender`s with each other
    , Show -- ^ Encode `Gender` as `String` through `show`
    )

-- | A person's name
newtype PersonName = PersonName Text
  deriving stock
    ( Eq   -- ^ Compare `PersonName`s with each other
    , Show -- ^ Encode `Page` as `String` through `show`
    )

{- |
Represents a person within the Star Wars universe

__Example__

@
Person
  { pName = PersonName "Luke Skywalker"
  , pHeight = Height 172
  , pMass = Mass 77.0
  , pHairColor = [BlondHair]
  , pSkinColor = [FairSkin]
  , pEyeColor = [BlueEye]
  , pBirthYear = BBY 19.0
  , pGender = Male
  , pHomeworldId = PlanetId 1
  , pFilmIds = [FilmId 1,FilmId 2,FilmId 3,FilmId 6]
  , pSpeciesIds = []
  , pVehicleIds = [VehicleId 14,VehicleId 30]
  , pStarshipIds = [StarshipId 12,StarshipId 22]
  , pCreatedAt = 2014-12-09 13:50:51.644 UTC
  , pEditedAt = 2014-12-20 21:17:56.891 UTC
  , pId = PersonId 1
  }
@
-}
data Person = Person
  { pName        :: PersonName     -- ^ Name of person
  , pHeight      :: Height         -- ^ Height of a person. Not known sometimes.
  , pMass        :: Mass           -- ^ A person's mass
  , pHairColor   :: [HairColor]    -- ^ Hair color combinations
  , pSkinColor   :: [SkinColor]    -- ^ Skin color combinations
  , pEyeColor    :: [EyeColor]     -- ^ Eye color combinations
  , pBirthYear   :: BirthYear      -- ^ Relative to before/after Battle of Yavin
  , pGender      :: Gender         -- ^ Genders in SW universe
  , pHomeworldId :: PlanetId       -- ^ Planet origin of person
  , pFilmIds     :: [FilmId]       -- ^ Person's appearance in films
  , pSpeciesIds  :: [SpeciesId]    -- ^ Species combinations
  , pVehicleIds  :: [VehicleId]    -- ^ Vehicles that this person piloted
  , pStarshipIds :: [StarshipId]   -- ^ Starships that this person piloted
  , pCreatedAt   :: UTCTime        -- ^ When this entry was created
  , pEditedAt    :: UTCTime        -- ^ Last time this entry was edited
  , pId          :: PersonId       -- ^ Person's ID in the database
  }
  deriving stock
    ( Eq   -- ^ Compare `Person`s with each other
    , Show -- ^ Encode `Person` as `String` through `show`
    )

--------------------------------------------------------------------------------
-- Instances

{- |
Decode a JSON value to a `Height`

A JSON value can be decoded to `Height` but must be one of these formats:

1. @"unknown"@
2. An integer literal encoded as a string. Cannot contain anything else, including
   whitespaces.

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
"bruh this isn't a number"
@

So, Sek Un, why is a number a string and not just a number literal? I don't know.
Ask the @swapi.dev@ maintainer. :)

-}
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
            -- TODO: Add result of `Right` that fails in error message
            _                     -> fail "Unexpected format for height"

-- | Encodes a `Height` to a string JSON value, either "unknown" or a number string.
instance ToJSON (Height :: Type) where
  toJSON :: Height -> Value
  toJSON height =
    case height of
      -- There's no `Integral a => a -> Text` apparently. So this is a hack for
      -- now. Relevant issue: https://github.com/haskell/text/issues/218
      Height n      -> String . Text.pack . show $ n
      UnknownHeight -> String "unknown"

{- |

Decode a JSON value to a `Mass`

A JSON value can be decoded to `Mass` but must be one of these formats:

1. @"unknown"@
2. An integer literal encoded as a string. Cannot contain anything else, including
   whitespaces.

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
"123kg"
"12 "
" 19"
"1 5"
"bruh this isn't a number"
@

-}
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

-- | Encodes a `Mass` to a string JSON value, either "unknown" or a number string.
instance ToJSON (Mass :: Type) where
  toJSON :: Mass -> Value
  toJSON mass =
    case mass of
      Mass numMass -> String . Text.pack . show $ numMass
      UnknownMass  -> String "unknown"

{- |

Decode a JSON value to a `BirthYear`

A JSON value can be decoded to `BirthYear` but must be one of these formats:

1. @"unknown"@
2. An integer/double literal encoded as a string, then appended with @"BBY"@ or
   @"ABY".

__Examples for #2__

__Good__

@
"20BBY"
"32ABY"
"unknown"
@

__Bad__

@
"20 ABY"
" 32ABY"
"19ABY "
"42"
"69 years"
@

-}
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

-- | Encodes a `BirthYear` to a string JSON value, either "unknown" or a number
-- string with a string @"ABY"@/@"BBY"@ suffix.
instance ToJSON (BirthYear :: Type) where
  toJSON :: BirthYear -> Value
  -- FIXME(sekun): Maybe use `showt` rather than `Text.pack . show`?
  toJSON (BBY years)      = String $ Text.pack $ mconcat [show years, "BBY"]
  toJSON (ABY years)      = String $ Text.pack $ mconcat [show years, "ABY"]
  toJSON UnknownBirthYear = String "unknown"

{- |

Decode a JSON value to a `Gender`

A JSON value can be decoded to `Gender` but must be one of these formats:

1. @"none"@
2. @"male"@
3. @"female"@
4. @"hermaphrodite"@

-}
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

-- | Encodes a `Gender` to a string JSON value
instance ToJSON (Gender :: Type) where
  toJSON :: Gender -> Value
  toJSON gender =
    case gender of
      Male          -> String "male"
      Female        -> String "female"
      Hermaphrodite -> String "hermaphrodite"
      NoGender      -> String "n/a"

-- | Decode a JSON value to a `PersonName`. Anything goes as long as it's a
-- string.
instance FromJSON (PersonName :: Type) where
  parseJSON :: Value -> Parser PersonName
  parseJSON =
   Aeson.withText "PersonName"
      $ \name ->
          pure $ PersonName name

-- | Encodes a `PersonName` to a string JSON value
instance ToJSON (PersonName :: Type) where
  toJSON :: PersonName -> Value
  toJSON (PersonName name) = String name

-- | Decodes a JSON object to a `Person`. As long as you meet the criteria for
-- each field then it should succeed.
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

-- | Encodes a `Person` to a JSON object
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

-- | Decodes the "index" of the `Person` resource
--
-- See: `Index`
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

-- | Encodes the "index" of a `Person` resource
--
-- See: `Index`
instance ToJSON (Index Person :: Type) where
  toJSON :: Index Person -> Value
  toJSON indexObject =
    Aeson.object
      [ "count"     .= iCount indexObject
      , "next"      .= iNextPage indexObject
      , "previous"  .= iPreviousPage indexObject
      , "results"   .= iResults indexObject
      ]


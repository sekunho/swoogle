{- |

This module contains:

1. Data types for a film
2. `aeson` instances for dealing with film JSON objects

You can decode/encode any data type in this module from and to JSON values.

__Decoding film JSON objects__

Let's say you have this JSON response from
@https://swapi.dev/api/film/1/?format=json@:

@
{
  "title": "A New Hope",
  "episode_id": 4,
  "opening_crawl": "It is a period of civil war.\r\nRebel spaceships, striking\r\nfrom a hidden base, have won\r\ntheir first victory against\r\nthe evil Galactic Empire.\r\n\r\nDuring the battle, Rebel\r\nspies managed to steal secret\r\nplans to the Empire's\r\nultimate weapon, the DEATH\r\nSTAR, an armored space\r\nstation with enough power\r\nto destroy an entire planet.\r\n\r\nPursued by the Empire's\r\nsinister agents, Princess\r\nLeia races home aboard her\r\nstarship, custodian of the\r\nstolen plans that can save her\r\npeople and restore\r\nfreedom to the galaxy....",
  "director": "George Lucas",
  "producer": "Gary Kurtz, Rick McCallum",
  "release_date": "1977-05-25",
  "characters": [
    "https://swapi.dev/api/people/1/",
    "https://swapi.dev/api/people/2/",
    "https://swapi.dev/api/people/3/",
    "https://swapi.dev/api/people/4/",
    "https://swapi.dev/api/people/5/",
    "https://swapi.dev/api/people/6/",
    "https://swapi.dev/api/people/7/",
    "https://swapi.dev/api/people/8/",
    "https://swapi.dev/api/people/9/",
    "https://swapi.dev/api/people/10/",
    "https://swapi.dev/api/people/12/",
    "https://swapi.dev/api/people/13/",
    "https://swapi.dev/api/people/14/",
    "https://swapi.dev/api/people/15/",
    "https://swapi.dev/api/people/16/",
    "https://swapi.dev/api/people/18/",
    "https://swapi.dev/api/people/19/",
    "https://swapi.dev/api/people/81/"
  ],
  "planets": [
    "https://swapi.dev/api/planets/1/",
    "https://swapi.dev/api/planets/2/",
    "https://swapi.dev/api/planets/3/"
  ],
  "starships": [
    "https://swapi.dev/api/starships/2/",
    "https://swapi.dev/api/starships/3/",
    "https://swapi.dev/api/starships/5/",
    "https://swapi.dev/api/starships/9/",
    "https://swapi.dev/api/starships/10/",
    "https://swapi.dev/api/starships/11/",
    "https://swapi.dev/api/starships/12/",
    "https://swapi.dev/api/starships/13/"
  ],
  "vehicles": [
    "https://swapi.dev/api/vehicles/4/",
    "https://swapi.dev/api/vehicles/6/",
    "https://swapi.dev/api/vehicles/7/",
    "https://swapi.dev/api/vehicles/8/"
  ],
  "species": [
    "https://swapi.dev/api/species/1/",
    "https://swapi.dev/api/species/2/",
    "https://swapi.dev/api/species/3/",
    "https://swapi.dev/api/species/4/",
    "https://swapi.dev/api/species/5/"
  ],
  "created": "2014-12-10T14:23:31.880000Z",
  "edited": "2014-12-20T19:49:45.256000Z",
  "url": "https://swapi.dev/api/films/1/"
}
@

And bind it to `sampleJSON`. Let's try decoding it!

>>> import Data.Aeson
>>> import SwapiClient.Resource.Film
>>> decode @Film sampleJSON
Film
  { fTitle = "A New Hope"
  , fEpisodeId = 4
  , fOpeningCrawl = OpeningCrawl "It is a period of civil war.\r\nRebel spaceships, striking\r\nfrom a hidden base, have won\r\ntheir first victory against\r\nthe evil Galactic Empire.\r\n\r\nDuring the battle, Rebel\r\nspies managed to steal secret\r\nplans to the Empire's\r\nultimate weapon, the DEATH\r\nSTAR, an armored space\r\nstation with enough power\r\nto destroy an entire planet.\r\n\r\nPursued by the Empire's\r\nsinister agents, Princess\r\nLeia races home aboard her\r\nstarship, custodian of the\r\nstolen plans that can save her\r\npeople and restore\r\nfreedom to the galaxy...."
  , fDirector = Director "George Lucas"
  , fProducers = [Producer "Gary Kurtz",Producer "Rick McCallum"]
  , fReleaseDate = 1977-05-25
  , fCharacters =
      [ PersonId 1
      , PersonId 2
      , PersonId 3
      , PersonId 4
      , PersonId 5
      , PersonId 6
      , PersonId 7
      , PersonId 8
      , PersonId 9
      , PersonId 10
      , PersonId 12
      , PersonId 13
      , PersonId 14
      , PersonId 15
      , PersonId 16
      , PersonId 18
      , PersonId 19
      , PersonId 81
      ]
  , fPlanets = [PlanetId 1,PlanetId 2,PlanetId 3]
  , fStarships =
      [ StarshipId 2
      , StarshipId 3
      , StarshipId 5
      , StarshipId 9
      , StarshipId 10
      , StarshipId 11
      , StarshipId 12
      , StarshipId 13
      ]
  , fVehicles = [VehicleId 4,VehicleId 6,VehicleId 7,VehicleId 8]
  , fSpecies = [SpeciesId 1,SpeciesId 2,SpeciesId 3,SpeciesId 4,SpeciesId 5]
  , fCreatedAt = 2014-12-10 14:23:31.88 UTC
  , fEditedAt = 2014-12-20 19:49:45.256 UTC
  , fId = FilmId 1
  }

1. Some string values were decoded to @newtypes@ to prevent mistaking it for just
   about any @Text@, @Int@, etc..
2. The JSON object's `url` field was parsed into a `FilmId`, since we won't
   deal with the URL itself to communicate with the API. Less error-prone this
   way. See `SwapiClient.Api` on how to communicate with @https://swapi.dev@.
3. Similar to the previous point, all IDs were parsed into their own ID types.

__Encoding `Film` to a JSON object__

You could also do the other way around, say you want to encode a `Film` as a
JSON object. Using the above `Film` record, let's assume we bound it to
`filmA`.

>>> import Data.Aeson
>>> import SwapiClient.Resource.Film
>>> encode filmA
{
  "characters": [
    "https://swapi.dev/api/people/1/",
    "https://swapi.dev/api/people/2/",
    "https://swapi.dev/api/people/3/",
    "https://swapi.dev/api/people/4/",
    "https://swapi.dev/api/people/5/",
    "https://swapi.dev/api/people/6/",
    "https://swapi.dev/api/people/7/",
    "https://swapi.dev/api/people/8/",
    "https://swapi.dev/api/people/9/",
    "https://swapi.dev/api/people/10/",
    "https://swapi.dev/api/people/12/",
    "https://swapi.dev/api/people/13/",
    "https://swapi.dev/api/people/14/",
    "https://swapi.dev/api/people/15/",
    "https://swapi.dev/api/people/16/",
    "https://swapi.dev/api/people/18/",
    "https://swapi.dev/api/people/19/",
    "https://swapi.dev/api/people/81/"
  ],
  "created": "2014-12-10T14:23:31.88Z",
  "director": "\"George Lucas\"",
  "edited": "2014-12-20T19:49:45.256Z",
  "episode_id": 4,
  "opening_crawl": "\"It is a period of civil war.\\r\\nRebel spaceships, striking\\r\\nfrom a hidden base, have won\\r\\ntheir first victory against\\r\\nthe evil Galactic Empire.\\r\\n\\r\\nDuring the battle, Rebel\\r\\nspies managed to steal secret\\r\\nplans to the Empire's\\r\\nultimate weapon, the DEATH\\r\\nSTAR, an armored space\\r\\nstation with enough power\\r\\nto destroy an entire planet.\\r\\n\\r\\nPursued by the Empire's\\r\\nsinister agents, Princess\\r\\nLeia races home aboard her\\r\\nstarship, custodian of the\\r\\nstolen plans that can save her\\r\\npeople and restore\\r\\nfreedom to the galaxy....\"",
  "planets": [
    "https://swapi.dev/api/planets/1/",
    "https://swapi.dev/api/planets/2/",
    "https://swapi.dev/api/planets/3/"
  ],
  "producer": "Gary Kurtz, Rick McCallum",
  "release_date": "1977-05-25",
  "species": [
    "https://swapi.dev/api/species/1/",
    "https://swapi.dev/api/species/2/",
    "https://swapi.dev/api/species/3/",
    "https://swapi.dev/api/species/4/",
    "https://swapi.dev/api/species/5/"
  ],
  "starships": [
    "https://swapi.dev/api/starships/2/",
    "https://swapi.dev/api/starships/3/",
    "https://swapi.dev/api/starships/5/",
    "https://swapi.dev/api/starships/9/",
    "https://swapi.dev/api/starships/10/",
    "https://swapi.dev/api/starships/11/",
    "https://swapi.dev/api/starships/12/",
    "https://swapi.dev/api/starships/13/"
  ],
  "title": "A New Hope",
  "url": "https://swapi.dev/api/films/1/",
  "vehicles": [
    "https://swapi.dev/api/vehicles/4/",
    "https://swapi.dev/api/vehicles/6/",
    "https://swapi.dev/api/vehicles/7/",
    "https://swapi.dev/api/vehicles/8/"
  ]
}

The result from this is of course filled with @\@ to escape a lot of symbols. I
removed it to make it more readable, but do keep this in mind.
-}
module SwapiClient.Resource.Film
  ( Film
      ( Film
      , fTitle
      , fEpisodeId
      , fOpeningCrawl
      , fDirector
      , fProducers
      , fReleaseDate
      , fCharacters
      , fPlanets
      , fStarships
      , fVehicles
      , fSpecies
      , fCreatedAt
      , fEditedAt
      , fId
      )
  , Director (Director)
  , Producer (Producer)
  , OpeningCrawl (OpeningCrawl)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson       (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Aeson       qualified as Aeson (object, withObject, withText)
import Data.Aeson.Types (Parser, Value (String))
import Data.Kind        (Type)
import Data.Text        (Text)
import Data.Text        qualified as Text (intercalate, splitOn)
import Data.Time        (Day, UTCTime)
import TextShow         (TextShow)
import TextShow         qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id   (FilmId, PersonId, PlanetId, SpeciesId, StarshipId,
                         VehicleId)
import SwapiClient.Page (Index (Index, iCount, iNextPage, iPreviousPage, iResults))

--------------------------------------------------------------------------------

{- |
Represents a film in the Star Wars franchise.

__Example__

> Film
>   { fTitle = "A New Hope"
>   , fEpisodeId = 4
>   , fOpeningCrawl = OpeningCrawl "It is a period of civil war.\r\nRebel spaceships, striking\r\nfrom a hidden base, have won\r\ntheir first victory against\r\nthe evil Galactic Empire.\r\n\r\nDuring the battle, Rebel\r\nspies managed to steal secret\r\nplans to the Empire's\r\nultimate weapon, the DEATH\r\nSTAR, an armored space\r\nstation with enough power\r\nto destroy an entire planet.\r\n\r\nPursued by the Empire's\r\nsinister agents, Princess\r\nLeia races home aboard her\r\nstarship, custodian of the\r\nstolen plans that can save her\r\npeople and restore\r\nfreedom to the galaxy...."
>   , fDirector = Director "George Lucas"
>   , fProducers = [Producer "Gary Kurtz",Producer "Rick McCallum"]
>   , fReleaseDate = 1977-05-25
>   , fCharacters =
>       [ PersonId 1
>       , PersonId 2
>       , PersonId 3
>       , PersonId 4
>       , PersonId 5
>       , PersonId 6
>       , PersonId 7
>       , PersonId 8
>       , PersonId 9
>       , PersonId 10
>       , PersonId 12
>       , PersonId 13
>       , PersonId 14
>       , PersonId 15
>       , PersonId 16
>       , PersonId 18
>       , PersonId 19
>       , PersonId 81
>       ]
>   , fPlanets = [PlanetId 1,PlanetId 2,PlanetId 3]
>   , fStarships =
>       [ StarshipId 2
>       , StarshipId 3
>       , StarshipId 5
>       , StarshipId 9
>       , StarshipId 10
>       , StarshipId 11
>       , StarshipId 12
>       , StarshipId 13
>       ]
>   , fVehicles = [VehicleId 4,VehicleId 6,VehicleId 7,VehicleId 8]
>   , fSpecies = [SpeciesId 1,SpeciesId 2,SpeciesId 3,SpeciesId 4,SpeciesId 5]
>   , fCreatedAt = 2014-12-10 14:23:31.88 UTC
>   , fEditedAt = 2014-12-20 19:49:45.256 UTC
>   , fId = FilmId 1
>   }
-}
data Film = Film
  { fTitle        :: Text          -- ^ A film's official title
  , fEpisodeId    :: Int           -- ^ The episode number in the SW series
  , fOpeningCrawl :: OpeningCrawl  -- ^ That scrolling wall of text before each one
  , fDirector     :: Director      -- ^ Film directors
  , fProducers    :: [Producer]    -- ^ Film producers
  , fReleaseDate  :: Day           -- ^ Release date of film IRL
  , fCharacters   :: [PersonId]    -- ^ Characters involved in the film
  , fPlanets      :: [PlanetId]    -- ^ Planets involved in the film
  , fStarships    :: [StarshipId]  -- ^ Starships involved in the film
  , fVehicles     :: [VehicleId]   -- ^ Vehicles involved in the film
  , fSpecies      :: [SpeciesId]   -- ^ Species involved in the film
  , fCreatedAt    :: UTCTime       -- ^ When this entry was created (API)
  , fEditedAt     :: UTCTime       -- ^ When this entry was last edited (API)
  , fId           :: FilmId        -- ^ Film ID in the API's database
  } deriving
      ( Eq   -- ^ Compare `Film`s with each other
      , Show -- ^ Encode `Film` as `String` through `show`
      )

-- TODO: Maybe consider producer and director as sum types?

-- | A producer of a film
newtype Producer = Producer Text
  -- ^ A wrapper to prevent mixing up a `Producer` with any `Text`.
  deriving stock
    ( Eq   -- ^ Compare `Producer`s with each other
    , Show -- ^ Encode `Producer` as `String` through `show`
    )
  deriving newtype TextShow
  {- ^
  Strips away the @newtype@, and encodes it as a `Text`; although this
  already is a `Text`.

  __Note__: This will be removed in the near future since `coerce` exists.

  __Example__

  >>> import TextShow
  >>> import SwapiClient.Resource.Film
  >>> showt (Producer "George Lucas")
  "Georce Lucas"
  -}

-- | A director of a film
newtype Director = Director Text
  -- ^ A wrapper to prevent mixing up a `Director` with any `Text`.
  deriving stock
    ( Eq   -- ^ Compare `Directory`s with each other
    , Show -- ^ Encode `Director` as `String` through `show`
    )
  deriving newtype TextShow
  {- ^
  Strips away the @newtype@, and encodes it as a `Text`; although this
  already is a `Text`.

  __Note__: This will be removed in the near future since `coerce` exists.

  __Example__

  >>> import TextShow
  >>> import SwapiClient.Resource.Film
  >>> showt (Director "George Lucas")
  "Georce Lucas"
  -}

-- | The opening crawl of a film
newtype OpeningCrawl = OpeningCrawl Text
  -- ^ A wrapper to prevent mixing up an `OpeningCrawl` with any `Text`.
  deriving stock
    ( Eq   -- ^ Compare `OpeningCrawl`s with each other
    , Show -- ^ Encode `OpeningCrawl` as `String` through `show`
    )
  deriving newtype TextShow
  {- ^
  Strips away the @newtype@, and encodes it as a `Text`; although this
  already is a `Text`.

  __Note__: This will be removed in the near future since `coerce` exists.

  __Example__

  >>> import TextShow
  >>> import SwapiClient.Resource.Film
  >>> showt (OpeningCrawl "Yeah, nah, not gonna put the full crawl here lol.")
  "Yeah, nah, not gonna put the full crawl here lol."
  -}

--------------------------------------------------------------------------------
-- TODO: Get rid of manual `ToJSON` instances, and use `DerivingVia`

-- | Decode a string JSON value to an `OpeningCrawl`.
instance FromJSON (OpeningCrawl :: Type) where
  parseJSON :: Value -> Parser OpeningCrawl
  parseJSON = Aeson.withText "OpeningCrawl" (pure . OpeningCrawl)

-- | Encodes an `OpeningCrawl` to a string JSON value.
instance ToJSON (OpeningCrawl :: Type) where
  toJSON :: OpeningCrawl -> Value
  toJSON = String . Text.Show.showt

-- | Decodes a string JSON value to a `Director`.
instance FromJSON (Director :: Type) where
  parseJSON :: Value -> Parser Director
  parseJSON = Aeson.withText "Director" (pure . Director)

-- | Encodes a `Directory` to a string JSON value.
instance ToJSON (Director :: Type) where
  toJSON :: Director -> Value
  toJSON = String . Text.Show.showt

-- | Decodes a string JSON value to a `Producer`.
instance FromJSON (Producer :: Type) where
  parseJSON :: Value -> Parser Producer
  parseJSON = Aeson.withText "Producer" (pure . Producer)

-- | Encodes a `Producer` to a string JSON value.
instance ToJSON (Producer :: Type) where
  toJSON :: Producer -> Value
  toJSON = String . Text.Show.showt

{- |
Decodes a comma-delimited string JSON value to a @[Producer]@.

__Why?__

Overrides the default @[a]@ instance that `aeson` implements. This is necessary
because @swapi.dev@ encodes a collection of producers as a comma-delimited string,
not a standard string. So this is just a way for me to decode it as @[Producer]@.
-}
instance {-# OVERLAPS #-} FromJSON ([Producer] :: Type) where
  parseJSON :: Value -> Parser [Producer]
  parseJSON =
    -- TODO: Refactor to use existing instance
    Aeson.withText "List of producers" (pure . map Producer . Text.splitOn ", ")

{- |
Encodes a @[Producer]@ as a comma-delimited string JSON value.

__Why?__

Overrides the default @[a]@ instance that `aeson` implements. This is necessary
because @swapi.dev@ encodes a collection of producers as a comma-delimited string,
not a standard string. So this is just a way for me to encode it as a comma-delimited
string.
-}
instance {-# OVERLAPS #-} ToJSON ([Producer] :: Type) where
  toJSON :: [Producer] -> Value
  toJSON =
    String . Text.intercalate ", " . map (\(Producer producer) -> producer)

-- | Decodes a JSON object to a `Film`.
instance FromJSON (Film :: Type) where
  parseJSON :: Value -> Parser Film
  parseJSON =
    Aeson.withObject "Film" $
      \filmObj ->
        Film
          <$> filmObj .: "title"
          <*> filmObj .: "episode_id"
          <*> filmObj .: "opening_crawl"
          <*> filmObj .: "director"
          <*> filmObj .: "producer"
          <*> filmObj .: "release_date"
          <*> filmObj .: "characters"
          <*> filmObj .: "planets"
          <*> filmObj .: "starships"
          <*> filmObj .: "vehicles"
          <*> filmObj .: "species"
          <*> filmObj .: "created"
          <*> filmObj .: "edited"
          <*> filmObj .: "url"

-- | Encodes a `Film` to a JSON object
instance ToJSON (Film :: Type) where
  toJSON :: Film -> Value
  toJSON film =
    Aeson.object
      [ "title"           .= fTitle film
      , "episode_id"      .= fEpisodeId film
      , "opening_crawl"   .= fOpeningCrawl film
      , "director"        .= fDirector film
      , "producer"        .= fProducers film
      , "release_date"    .= fReleaseDate film
      , "characters"      .= fCharacters film
      , "planets"         .= fPlanets film
      , "starships"       .= fStarships film
      , "vehicles"        .= fVehicles film
      , "species"         .= fSpecies film
      , "created"         .= fCreatedAt film
      , "edited"          .= fEditedAt film
      , "url"             .= fId film
      ]

-- | Decodes an index JSON object to an @Index Film@.
instance FromJSON (Index Film :: Type) where
  parseJSON :: Value -> Parser (Index Film)
  parseJSON =
    Aeson.withObject "Index" $
      \indexObject ->
        Index
          <$> indexObject .: "count"
          <*> indexObject .: "next"
          <*> indexObject .: "previous"
          <*> indexObject .: "results"

-- | Encodes an @Index Film@ to an index JSON object.
instance ToJSON (Index Film :: Type) where
  toJSON :: Index Film -> Value
  toJSON indexObject =
    Aeson.object
      [ "count"     .= iCount indexObject
      , "next"      .= iNextPage indexObject
      , "previous"  .= iPreviousPage indexObject
      , "results"   .= iResults indexObject
      ]

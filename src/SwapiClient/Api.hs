{-# LANGUAGE DataKinds #-}

module SwapiClient.Api
  ( listPeople
  , listFilms
  , listStarships
  , listVehicles
  , listSpecies
  , listPlanets
  , getPerson
  , getFilm
  , getStarship
  , getVehicle
  , getSpecies
  , getPlanet
  , searchPeople
  , searchFilms
  , searchStarships
  , searchVehicles
  , searchSpecies
  , searchPlanets
  , eitherListPeople
  , eitherListFilms
  , eitherListStarships
  , eitherListVehicles
  , eitherListSpecies
  , eitherListPlanets
  , eitherGetPerson
  , eitherGetFilm
  , eitherGetStarship
  , eitherGetVehicle
  , eitherGetSpecies
  , eitherGetPlanet
  , eitherSearchPeople
  , eitherSearchFilms
  , eitherSearchStarships
  , eitherSearchVehicles
  , eitherSearchSpecies
  , eitherSearchPlanets
  ) where

--------------------------------------------------------------------------------

import Control.Exception             (throwIO)
import Data.Aeson                    (FromJSON)
import Data.Aeson                    qualified as Aeson (decodeStrict,
                                                         eitherDecodeStrict)
import Data.ByteString               (ByteString)
import Data.Functor                  ((<&>))
import Data.Text                     (Text)
import Network.HTTP.Req              (GET (GET), NoReqBody (NoReqBody), Option,
                                      Req, Url, (/:), (=:))
import Network.HTTP.Req              qualified as Req (bsResponse,
                                                       defaultHttpConfig, req,
                                                       responseBody, runReq)
import TextShow                      qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id
import SwapiClient.Internal.Page     (Index, Page (NoPage, Page))
import SwapiClient.Internal.Resource (Resource (FilmResource, PeopleResource, PlanetResource, SpeciesResource, StarshipResource, VehicleResource))
import SwapiClient.Internal.Resource qualified as Resource (resourceToUrl)
import SwapiClient.Resource.Film
import SwapiClient.Resource.Person   (Person)
import SwapiClient.Resource.Planet   (Planet)
import SwapiClient.Resource.Species  (SpeciesType)
import SwapiClient.Resource.Starship
import SwapiClient.Resource.Vehicle  (Vehicle)

--------------------------------------------------------------------------------
-- People

-- | Fetches a list of people given a `Page`.
--
-- >>> listPeople (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
listPeople :: Page -> IO (Index Person)
listPeople page =
  fetchPage PeopleResource page >>=
    \case
      Just peopleIndex -> pure peopleIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single person associated with the provided `PersonId`.
--
-- >>> getPerson (PersonId 1)
-- Person { ... }
getPerson :: PersonId -> IO Person
getPerson (PersonId personId) =
  fetchOne PeopleResource personId >>=
    \case
      Just person -> pure person
      Nothing     -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a person's name; results are paginated.
--
-- >>> searchPeople "r2d2" (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
searchPeople :: Text -> Page -> IO (Index Person)
searchPeople query page =
  search PeopleResource query page >>=
    \case
      Just personIndex -> pure personIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a list of people given a `Page`.
--
-- >>> eitherListPeople (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListPeople :: Page -> IO (Either String (Index Person))
eitherListPeople = eitherFetchPage PeopleResource

-- | Fetches a single person associated with the provided `PersonId`.
--
-- >>> eitherGetPerson (PersonId 1)
-- Right $ Person { ... }
eitherGetPerson :: PersonId -> IO (Either String Person)
eitherGetPerson (PersonId personId) = eitherFetchOne PeopleResource personId

-- | Searches for a person's name; results are paginated.
--
-- >>> searchPeople "r2d2" (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchPeople :: Text -> Page -> IO (Either String (Index Person))
eitherSearchPeople = eitherSearch PeopleResource

--------------------------------------------------------------------------------
-- Film

-- | Fetches a list of films given a `Page`.
--
-- >>> listFilms (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Film {...} ]
--   }
listFilms :: Page -> IO (Index Film)
listFilms page =
  fetchPage FilmResource page >>=
    \case
      Just filmIndex -> pure filmIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single film associated with the provided `FilmId`.
--
-- >>> getFilm (FilmId 1)
-- Film { ... }
getFilm :: FilmId -> IO Film
getFilm (FilmId filmId) =
  fetchOne FilmResource filmId >>=
    \case
      Just film -> pure film
      Nothing   -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a film's name; results are paginated.
--
-- >>> searchFilms "empire" (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
searchFilms :: Text -> Page -> IO (Index Film)
searchFilms query page =
  search FilmResource query page >>=
    \case
      Just filmIndex -> pure filmIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a list of films given a `Page`.
--
-- >>> eitherListFilms (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Film {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListFilms :: Page -> IO (Either String (Index Film))
eitherListFilms = eitherFetchPage FilmResource

-- | Fetches a single film associated with the provided `FilmId`.
--
-- >>> eitherGetFilm (FilmId 1)
-- Right $ Film { ... }
eitherGetFilm :: FilmId -> IO (Either String Film)
eitherGetFilm (FilmId filmId) = eitherFetchOne FilmResource filmId

-- | Searches for a film's name; results are paginated.
--
-- >>> eitherSearchFilm "the empire strikes" (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Film {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchFilms :: Text -> Page -> IO (Either String (Index Film))
eitherSearchFilms = eitherSearch FilmResource

--------------------------------------------------------------------------------
-- Starship

-- | Fetches a list of starships given a `Page`.
--
-- >>> listStarships (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
listStarships :: Page -> IO (Index Starship)
listStarships page =
  fetchPage StarshipResource page >>=
    \case
      Just starshipIndex -> pure starshipIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single starship associated with the provided `StarshipId`.
--
-- >>> getStarship (StarshipId 1)
-- Starship { ... }
getStarship :: StarshipId -> IO Starship
getStarship (StarshipId starshipId) =
  fetchOne StarshipResource starshipId >>=
    \case
      Just starship -> pure starship
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a starship's name; results are paginated.
--
-- >>> searchStarships "millennium falcon" (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
searchStarships :: Text -> Page -> IO (Index Starship)
searchStarships query page =
  search StarshipResource query page >>=
    \case
      Just starshipIndex -> pure starshipIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a list of starships given a `Page`.
--
-- >>> eitherListStarships (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListStarships :: Page -> IO (Either String (Index Starship))
eitherListStarships = eitherFetchPage StarshipResource

-- | Fetches a single starship associated with the provided `StarshipId`.
--
-- >>> eitherGetStarship (StarshipId 1)
-- Right $ Starship { ... }
eitherGetStarship :: StarshipId -> IO (Either String Starship)
eitherGetStarship (StarshipId starshipId) =
  eitherFetchOne StarshipResource starshipId

-- | Searches for a starship's name; results are paginated.
--
-- >>> eitherSearchStarship "millennium falcon" (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchStarships :: Text -> Page -> IO (Either String (Index Starship))
eitherSearchStarships = eitherSearch StarshipResource

--------------------------------------------------------------------------------
-- Vehicle

-- | Fetches a list of vehicles given a `Page`.
--
-- >>> listVehicles (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Vehicle {...} ]
--   }
listVehicles :: Page -> IO (Index Vehicle)
listVehicles page =
  fetchPage VehicleResource page >>=
    \case
      Just vehicleIndex -> pure vehicleIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single vehicle associated with the provided `VehicleId`.
--
-- >>> getVehicle (VehicleId 6)
-- Vehicle { ... }
getVehicle :: VehicleId -> IO Vehicle
getVehicle (VehicleId vehicleId) =
  fetchOne VehicleResource vehicleId >>=
      \case
      Just vehicleIndex -> pure vehicleIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a vehicle's name; results are paginated.
--
-- >>> searchVehicles "tie/ln" (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Vehicle {...} ]
--   }
searchVehicles :: Text -> Page -> IO (Index Vehicle)
searchVehicles query page =
  search VehicleResource query page >>=
    \case
      Just vehicleIndex -> pure vehicleIndex
      Nothing           -> throwIO (userError "Bruh")

-- | Fetches a list of vehicles given a `Page`.
--
-- >>> eitherListVehicles (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Vehicle {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListVehicles :: Page -> IO (Either String (Index Vehicle))
eitherListVehicles = eitherFetchPage VehicleResource

-- | Fetches a single vehicle associated with the provided `VehicleId`.
--
-- >>> eitherGetVehicle (VehicleId 6)
-- Right $ Vehicle { ... }
eitherGetVehicle :: VehicleId -> IO (Either String Vehicle)
eitherGetVehicle (VehicleId vehicleId) =
  eitherFetchOne VehicleResource vehicleId

-- | Searches for a vehicle's name; results are paginated.
--
-- >>> eitherSearchVehicle "millennium falcon" (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Vehicle {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchVehicles :: Text -> Page -> IO (Either String (Index Vehicle))
eitherSearchVehicles = eitherSearch VehicleResource

--------------------------------------------------------------------------------
-- Species

-- | Fetches a list of speciess given a `Page`.
--
-- >>> listSpecies (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Species {...} ]
--   }
listSpecies :: Page -> IO (Index SpeciesType)
listSpecies page =
  fetchPage SpeciesResource page >>=
    \case
      Just species -> pure species
      Nothing      -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single species associated with the provided `SpeciesId`.
--
-- >>> getSpecies (SpeciesId 6)
-- Species { ... }
getSpecies :: SpeciesId -> IO SpeciesType
getSpecies (SpeciesId speciesId) =
  fetchOne SpeciesResource speciesId >>=
    \case
      Just species -> pure species
      Nothing      -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a species' name; results are paginated.
--
-- >>> searchSpecies "human" (Page 1)
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Species {...} ]
--   }
searchSpecies :: Text -> Page -> IO (Index SpeciesType)
searchSpecies query page =
  search SpeciesResource query page >>=
    \case
      Just species -> pure species
      Nothing      -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a list of speciess given a `Page`.
--
-- >>> eitherListSpecies (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Species {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListSpecies :: Page -> IO (Either String (Index SpeciesType))
eitherListSpecies = eitherFetchPage SpeciesResource

-- | Fetches a single species associated with the provided `SpeciesId`.
--
-- >>> eitherGetSpecies (SpeciesId 6)
-- Right $ Species { ... }
eitherGetSpecies :: SpeciesId -> IO (Either String SpeciesType)
eitherGetSpecies (SpeciesId speciesId) =
  eitherFetchOne SpeciesResource speciesId

-- | Searches for a species' name; results are paginated.
--
-- >>> eitherSearchSpecies "human" (Page 1)
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Species {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchSpecies :: Text -> Page -> IO (Either String (Index SpeciesType))
eitherSearchSpecies = eitherSearch SpeciesResource

--------------------------------------------------------------------------------
-- Planet

-- | Fetches a list of planets given a `Page`.
--
-- >>> listPlanets (Page 1)`
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Planet {...} ]
--   }
listPlanets :: Page -> IO (Index Planet)
listPlanets page =
  fetchPage PlanetResource page >>=
    \case
      Just planetIndex -> pure planetIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a single planet associated with the provided `PlanetId`.
--
-- >>> getPlanet (PlanetId 6)`
-- Planet { ... }
getPlanet :: PlanetId -> IO Planet
getPlanet (PlanetId planetId) =
  fetchOne PlanetResource planetId >>=
    \case
      Just planet -> pure planet
      Nothing     -> throwIO (userError "TODO: Make this error message better")

-- | Searches for a planet's name; results are paginated.
--
-- >>> searchPlanets "human" (Page 1)`
-- Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Planet {...} ]
--   }
searchPlanets :: Text -> Page -> IO (Index Planet)
searchPlanets query page =
  search PlanetResource query page >>=
    \case
      Just planetIndex -> pure planetIndex
      Nothing -> throwIO (userError "TODO: Make this error message better")

-- | Fetches a list of planets given a `Page`.
--
-- >>> eitherListPlanets (Page 1)`
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Planet {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherListPlanets :: Page -> IO (Either String (Index Planet))
eitherListPlanets = eitherFetchPage PlanetResource

-- | Fetches a single planet associated with the provided `PlanetId`.
--
-- >>> eitherGetPlanet (PlanetId 6)`
--
-- `Right $ Planet { ... }`
eitherGetPlanet :: PlanetId -> IO (Either String Planet)
eitherGetPlanet (PlanetId planetId) =
  eitherFetchOne PlanetResource planetId

-- | Searches for a planet's name; results are paginated.
--
-- >>> eitherSearchPlanets "human" (Page 1)`
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Planet {...} ]
--   }
--
-- If the page provided is `NoPage`, it gives back @Left "This is an empty page"@.
eitherSearchPlanets :: Text -> Page -> IO (Either String (Index Planet))
eitherSearchPlanets = eitherSearch PlanetResource

--------------------------------------------------------------------------------
-- Utils

-- | An HTTP request with `defaultHttpConfig`
runReq :: Req a -> IO a
runReq = Req.runReq Req.defaultHttpConfig

-- | Basic `GET` request
get :: Option scheme -> Url scheme -> IO ByteString
get params url =
  runReq $
    Req.req GET url NoReqBody Req.bsResponse ("format" =: ("json" :: Text) <> params)
      <&> Req.responseBody

-- | Fetches one SWAPI resource
fetchOne
  :: FromJSON a
  => Resource
  -> Word -- Page number, should probably be `Word` instead though.
  -> IO (Maybe a)
fetchOne resource resourceId =
  Aeson.decodeStrict <$>
    get mempty (Resource.resourceToUrl resource /: Text.Show.showt resourceId)

fetchPage
  :: FromJSON (Index a)
  => Resource
  -> Page
  -> IO (Maybe (Index a))
fetchPage resource =
  \case
    Page page ->
      Aeson.decodeStrict <$>
        get ("page" =: Text.Show.showt page) (Resource.resourceToUrl resource)

    NoPage -> pure Nothing

search
  :: FromJSON (Index a)
  => Resource            -- Resource to search through
  -> Text                -- Search query
  -> Page                -- Page number of search results
  -> IO (Maybe (Index a))
search resource query =
  \case
    Page page ->
      Aeson.decodeStrict <$>
        get
          ("page" =: Text.Show.showt page <> "search" =: query)
          (Resource.resourceToUrl resource)

    NoPage -> pure Nothing

eitherFetchOne
 :: FromJSON a
 => Resource -- Resource to fetch one of
 -> Word -- Resource ID number
 -> IO (Either String a)
eitherFetchOne resource resourceId =
  Aeson.eitherDecodeStrict <$>
    get mempty (Resource.resourceToUrl resource /: Text.Show.showt resourceId)

eitherFetchPage
  :: FromJSON a
  => Resource -- Resource to fetch a list of
  -> Page     -- Page number of results
  -> IO (Either String a)
eitherFetchPage resource =
  \case
    Page page ->
      Aeson.eitherDecodeStrict <$>
        get ("page" =: Text.Show.showt page) (Resource.resourceToUrl resource)

    NoPage -> pure (Left "You can't fetch results from an empty page.")

-- TODO: URL encode `query`
eitherSearch
  :: FromJSON a
  => Resource -- Resource to search through
  -> Text     -- Search query
  -> Page     -- Page number of search results
  -> IO (Either String a)
eitherSearch resource query =
  \case
    Page page ->
      Aeson.eitherDecodeStrict <$>
        get
          ("page" =: Text.Show.showt page <> "search" =: query)
          (Resource.resourceToUrl resource)

    NoPage -> pure (Left "You can't fetch search results from an empty page.")

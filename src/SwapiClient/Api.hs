{-# LANGUAGE FlexibleContexts #-}
{-# language DataKinds #-}

module SwapiClient.Api
  ( listPeople
--  , listFilms
--  , listStarships
  , getPerson
--  , getFilm
--  , getStarship
  , searchPeople
  , eitherListPeople
--  , eitherListFilms
--  , eitherListStarships
  , eitherGetPerson
--  , eitherGetFilm
--  , eitherGetStarship
  , eitherSearchPeople
--  , eitherSearchFilms
--  , eitherSearchStarships
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson (decodeStrict, eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Req
  ( Req
  , GET (GET)
  , NoReqBody (NoReqBody)
  , (/:)
  , (=:)
  , Url
  , Option
  )
import Network.HTTP.Req qualified as Req
  ( req
  , runReq
  , defaultHttpConfig
  , bsResponse
  , responseBody
  )
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id
import SwapiClient.Page (Page (Page, NoPage), Index)
import SwapiClient.Person (Person)
import SwapiClient.Url
import SwapiClient.Url qualified as Url (fromResource)

--------------------------------------------------------------------------------
-- People

-- | Fetches a list of people given a `Page`.
--
-- `ghci> listPeople (Page 1)`
--
-- ```haskell
-- Just $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Nothing`.
listPeople :: Page -> IO (Maybe (Index Person))
listPeople = fetchPage People

-- | Fetches a single person associated with the provided `PersonId`.
--
-- `ghci> getPerson (PersonId 1)`
--
-- `Just $ Person { ... }`
getPerson :: PersonId -> IO (Maybe Person)
getPerson (PersonId personId) = fetchOne People personId

-- | Searches for a person's name; results are paginated.
--
-- `ghci> searchPeople "r2d2" (Page 1)`
--
-- ```haskell
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Nothing`.
searchPeople :: Text -> Page -> IO (Maybe (Index Person))
searchPeople = search People

-- | Fetches a list of people given a `Page`.
--
-- `ghci> eitherListPeople (Page 1)`
--
-- ```haskell
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Left "This is an empty page"`.
eitherListPeople :: Page -> IO (Either String (Index Person))
eitherListPeople = eitherFetchPage People

-- | Fetches a single person associated with the provided `PersonId`.
--
-- `ghci> eitherGetPerson (PersonId 1)`
--
-- `Right $ Person { ... }`
eitherGetPerson :: PersonId -> IO (Either String Person)
eitherGetPerson (PersonId personId) = eitherFetchOne People personId

-- | Searches for a person's name; results are paginated.
--
-- `ghci> searchPeople "r2d2" (Page 1)`
--
-- ```haskell
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Person {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Left "This is an empty page"`.
eitherSearchPeople :: Text -> Page -> IO (Either String (Index Person))
eitherSearchPeople = eitherSearch People

--------------------------------------------------------------------------------
-- Film

-- | Fetches a list of films given a `Page`.
--
-- `ghci> listFilms (Page 1)`
--
-- ```haskell
-- Just $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Film {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Nothing`.
-- listFilms :: Page -> IO (Maybe (Index Film))
-- listFilms page =
--   runReq (getPage (Url.swapiBin /: "films") page)

-- | Fetches a list of films given a `Page`.
--
-- `ghci> eitherListPeople (Page 1)`
--
-- ```haskell
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Film {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Left "This is an empty page"`.
-- eitherListFilms :: Page -> IO (Either String (Index Film))
-- eitherListFilms page =
--   runReq (eitherGetPage (Url.swapiBin /: "films") page)

-- | Fetches a single film associated with the provided `FilmId`.
--
-- `ghci> getFilm (FilmId 1)`
--
-- `Just $ Film { ... }`
-- getFilm :: FilmId -> IO (Maybe Film)
-- getFilm filmId =
--   runReq (get (Url.swapiBin /: "films" /: Text.Show.showt filmId))

--------------------------------------------------------------------------------
-- Starship

-- | Fetches a list of starships given a `Page`.
--
-- `ghci> listStarships (Page 1)`
--
-- ```haskell
-- Just $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Nothing`.
-- listStarships :: Page -> IO (Maybe (Index Starship))
-- listStarships page =
--   runReq (getPage (Url.swapiBin /: "starships") page)

-- | Fetches a list of starships given a `Page`.
--
-- `ghci> eitherListStarships (Page 1)`
--
-- ```haskell
-- Right $ Index
--   { iCount = 1
--   , iNextPage = NoPage
--   , iPreviousPage = NoPage
--   , iResults = [ Starship {...} ]
--   }
-- ```
--
-- If the page provided is `NoPage`, it gives back `Left "This is an empty page"`.
-- eitherListStarships :: Page -> IO (Either String (Index Starship))
-- eitherListStarships page =
--   runReq (eitherGetPage (Url.swapiBin /: "starships") page)

-- | Fetches a single starship associated with the provided `StarshipId`.
--
-- `ghci> getStarship (StarshipId 1)`
--
-- `Just $ Starship { ... }`
-- getStarship :: StarshipId -> IO (Maybe Starship)
-- getStarship starshipId =
--   runReq (get (Url.swapiBin /: "starships" /: Text.Show.showt starshipId))


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
  -> Int           -- Page number, should probably be `Word` instead though.
  -> IO (Maybe a)
fetchOne resource resourceId =
  Aeson.decodeStrict <$>
    get mempty (Url.fromResource resource /: Text.Show.showt resourceId)

fetchPage
  :: FromJSON (Index a)
  => Resource
  -> Page
  -> IO (Maybe (Index a))
fetchPage resource =
  \case
    Page page ->
      Aeson.decodeStrict <$>
        get ("page" =: Text.Show.showt page) (Url.fromResource resource)

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
          (Url.fromResource resource)

    NoPage -> pure Nothing

eitherFetchOne
 :: FromJSON a
 => Resource -- Resource to fetch one of
 -> Int      -- Resource ID number
 -> IO (Either String a)
eitherFetchOne resource resourceId =
  Aeson.eitherDecodeStrict <$>
    get mempty (Url.fromResource resource /: Text.Show.showt resourceId)

eitherFetchPage
  :: FromJSON a
  => Resource -- Resource to fetch a list of
  -> Page     -- Page number of results
  -> IO (Either String a)
eitherFetchPage resource =
  \case
    Page page ->
      Aeson.eitherDecodeStrict <$>
        get ("page" =: Text.Show.showt page) (Url.fromResource resource)

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
          (Url.fromResource resource)

    NoPage -> pure (Left "You can't fetch search results from an empty page.")

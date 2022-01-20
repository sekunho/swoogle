module SwapiClient.Api
  ( listPeople
  , listFilms
  , eitherListPeople
  , eitherListFilms
  , getPerson
  , getFilm
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson (decodeStrict, eitherDecodeStrict)
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Req
  ( Req
  , GET (GET)
  , NoReqBody (NoReqBody)
  , (/:)
  , (=:)
  , Url
  , MonadHttp
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

import SwapiClient.Id (PersonId, FilmId)
import SwapiClient.Page (Page (Page, NoPage), Index)
import SwapiClient.Person (Person)
import SwapiClient.Film (Film)
import SwapiClient.Url qualified as Url (swapiBin)

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
listPeople page =
  runReq (getPage (Url.swapiBin /: "people") page)

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
eitherListPeople page =
  runReq (eitherGetPage (Url.swapiBin /: "people") page)

-- | Fetches a single person associated with the provided `PersonId`.
--
-- `ghci> getPerson (PersonId 1)`
--
-- `Just $ Person { ... }`
getPerson :: PersonId -> IO (Maybe Person)
getPerson personId =
  runReq (get (Url.swapiBin /: "people" /: Text.Show.showt personId))

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
listFilms :: Page -> IO (Maybe (Index Film))
listFilms page =
  runReq (getPage (Url.swapiBin /: "films") page)

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
eitherListFilms :: Page -> IO (Either String (Index Film))
eitherListFilms page =
  runReq (eitherGetPage (Url.swapiBin /: "films") page)

-- | Fetches a single film associated with the provided `FilmId`.
--
-- `ghci> getFilm (FilmId 1)`
--
-- `Just $ Film { ... }`
getFilm :: FilmId -> IO (Maybe Film)
getFilm filmId =
  runReq (get (Url.swapiBin /: "films" /: Text.Show.showt filmId))

--------------------------------------------------------------------------------
-- Utils

-- | An HTTP request with `defaultHttpConfig`
runReq :: Req a -> IO a
runReq = Req.runReq Req.defaultHttpConfig

-- | Fetches one or more of a resource
get :: (MonadHttp m, FromJSON a) => Url scheme -> m (Maybe a)
get url =
  Req.req GET url NoReqBody Req.bsResponse ("format" =: ("json" :: Text))
    <&> Req.responseBody
    <&> Aeson.decodeStrict

-- | Like `get` except with pagination
getPage :: (MonadHttp m, FromJSON a) => Url scheme -> Page -> m (Maybe a)
getPage url page =
  case page of
    Page pageNum ->
      Req.req
        GET
        url
        NoReqBody
        Req.bsResponse
        ("format" =: ("json" :: Text) <> "page" =: Text.Show.showt pageNum)
      <&> Req.responseBody
      <&> Aeson.decodeStrict

    NoPage -> pure Nothing

-- | An `Either` version of `getPage`
eitherGetPage
  :: (MonadHttp m, FromJSON a)
  => Url scheme
  -> Page
  -> m (Either String a)
eitherGetPage url page =
    case page of
    Page pageNum ->
      Req.req
        GET
        url
        NoReqBody
        Req.bsResponse
        ("format" =: ("json" :: Text) <> "page" =: Text.Show.showt pageNum)
      <&> Req.responseBody
      <&> Aeson.eitherDecodeStrict

    NoPage -> pure (Left "This is an empty page")

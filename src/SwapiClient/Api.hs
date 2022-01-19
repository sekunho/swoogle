module SwapiClient.Api
  ( listPeople
  , getPerson
  , listFilms
  , getFilm
  ) where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson (decodeStrict)
import Data.Functor ((<&>))
import Network.HTTP.Req
  ( Req
  , GET (GET)
  , NoReqBody (NoReqBody)
  , (/:)
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
import SwapiClient.Page (Index)
import SwapiClient.Person (Person)
import SwapiClient.Film (Film)
import SwapiClient.Url qualified as Url (swapiBin)

--------------------------------------------------------------------------------
-- People

listPeople :: IO (Maybe (Index Person))
listPeople =
   runReq (get (Url.swapiBin /: "people"))

getPerson :: PersonId -> IO (Maybe Person)
getPerson personId =
  runReq $
    get (Url.swapiBin /: "people" /: Text.Show.showt personId)

--------------------------------------------------------------------------------
-- Film

listFilms :: IO (Maybe (Index Film))
listFilms =
  runReq $
    get (Url.swapiBin /: "films")

getFilm :: FilmId -> IO (Maybe Film)
getFilm filmId =
  runReq $
    get (Url.swapiBin /: "films" /: Text.Show.showt filmId)

--------------------------------------------------------------------------------
-- Utils

-- | An HTTP request with `defaultHttpConfig`
runReq :: Req a -> IO a
runReq = Req.runReq Req.defaultHttpConfig

get :: (MonadHttp m, FromJSON a) => Url scheme -> m (Maybe a)
get url =
  Req.req GET url NoReqBody Req.bsResponse mempty
    <&> Req.responseBody
    <&> Aeson.decodeStrict

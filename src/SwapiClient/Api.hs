module SwapiClient.Api
  ( listPeople
  , getPerson
  ) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (decodeStrict)
import Data.Functor ((<&>))
import Network.HTTP.Req (Req, GET (GET), NoReqBody (NoReqBody), (/:))
import Network.HTTP.Req qualified as Req
  ( req
  , runReq
  , defaultHttpConfig
  , bsResponse
  , responseBody
  )
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Id (PersonId)
import SwapiClient.Page (Index)
import SwapiClient.Person (Person)
import SwapiClient.Url qualified as Url (swapiBin)

--------------------------------------------------------------------------------
-- People

listPeople :: IO (Maybe (Index Person))
listPeople =
   runReq $
    Req.req GET (Url.swapiBin /: "people") NoReqBody Req.bsResponse mempty
    <&> Req.responseBody
    <&> Aeson.decodeStrict

getPerson :: PersonId -> IO (Maybe Person)
getPerson personId =
  runReq $
    Req.req
      GET
      (Url.swapiBin /: "people" /: Text.Show.showt personId)
      NoReqBody
      Req.bsResponse
      mempty
    <&> Req.responseBody
    <&> Aeson.decodeStrict

--------------------------------------------------------------------------------
-- Utils

-- | An HTTP request with `defaultHttpConfig`
runReq :: Req a -> IO a
runReq = Req.runReq Req.defaultHttpConfig

module SwapiClient.Api where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (decodeStrict)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), (/:))
import Network.HTTP.Req qualified as Req
  ( req
  , runReq
  , defaultHttpConfig
  , bsResponse
  , responseBody
  )



--------------------------------------------------------------------------------

import SwapiClient.Page (Index)
import SwapiClient.Person (Person)
import SwapiClient.Url qualified as Url (swapiBin)

--------------------------------------------------------------------------------

listPeople :: IO (Maybe (Index Person))
listPeople =
  Aeson.decodeStrict
    <$> (Req.runReq Req.defaultHttpConfig $
    Req.req GET (Url.swapiBin /: "people") NoReqBody Req.bsResponse mempty >>=
      pure . Req.responseBody)

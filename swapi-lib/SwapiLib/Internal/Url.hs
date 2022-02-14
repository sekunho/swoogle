{-# LANGUAGE DataKinds #-}

module SwapiLib.Internal.Url
  ( baseUrl
  , swapiBin
  , swapiDomain
  ) where

--------------------------------------------------------------------------------

import Data.Text        (Text)
import Network.HTTP.Req (Scheme (Https), Url, (/:))
import Network.HTTP.Req qualified as Req (https)

--------------------------------------------------------------------------------

-- Functions

-- TODO: Reuse `swapiDomain` in `baseUrl`
baseUrl :: Text
baseUrl = "https://swapi.dev/api/"

swapiDomain :: Text
swapiDomain = "swapi.dev"

swapiBin :: Url 'Https
swapiBin = Req.https swapiDomain /: "api"


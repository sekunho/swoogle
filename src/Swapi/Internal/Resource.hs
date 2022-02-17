{-# LANGUAGE DataKinds #-}

module Swapi.Internal.Resource where

--------------------------------------------------------------------------------

import Data.Text                (Text)
import Data.Text                qualified as Text (append)
import Network.HTTP.Req         (Scheme (Https), Url, (/:))


--------------------------------------------------------------------------------

import Swapi.Internal.Url qualified as Url (baseUrl, swapiBin)

--------------------------------------------------------------------------------

data Resource
  = PeopleResource
  | FilmResource
  | StarshipResource
  | VehicleResource
  | SpeciesResource
  | PlanetResource
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

resourceToUrl :: Resource -> Url 'Https
resourceToUrl = (/:) Url.swapiBin .
  \case
    PeopleResource   -> "people"
    FilmResource     -> "films"
    StarshipResource -> "starships"
    VehicleResource  -> "vehicles"
    SpeciesResource  -> "species"
    PlanetResource   -> "planets"

-- TODO: Rename `resourceUrl` to `toUrl`?
resourceToUrlText :: Resource -> Text
resourceToUrlText resource =
  Text.append Url.baseUrl $
    case resource of
      PeopleResource   -> "people/"
      FilmResource     -> "films/"
      StarshipResource -> "starships/"
      VehicleResource  -> "vehicles/"
      SpeciesResource  -> "species/"
      PlanetResource   -> "planets/"

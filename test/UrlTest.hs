module UrlTest
  ( spec_resourceUrl
  , spec_getId
  , spec_urlToUrlData
  , spec_urlDataToUrl
  ) where

--------------------------------------------------------------------------------

import Data.Map.Strict         (Map)
import Data.Map.Strict         qualified as Map (empty, fromList)
import Data.Text               (Text)
import Test.Hspec              (Spec)
import Test.Hspec              qualified as Hspec (describe, it, shouldBe)

--------------------------------------------------------------------------------

import Swapi.Internal.Resource (Resource (FilmResource, PeopleResource, PlanetResource, SpeciesResource, StarshipResource, VehicleResource))
import Swapi.Internal.Resource qualified as Resource (resourceToUrlText)
import Swapi.Internal.UrlData  (UrlData (UrlData, udParams, udSubdir))
import Swapi.Internal.UrlData  qualified as UrlData (fromUrlText, getId,
                                                     toUrlText)
import Swapi.Url               qualified as Url (baseUrl)

--------------------------------------------------------------------------------
-- Specs

spec_resourceUrl :: Spec
spec_resourceUrl =
  Hspec.describe "resourceUrl" $ do
    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText PeopleResource `Hspec.shouldBe` (Url.baseUrl <> "people/")

    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText FilmResource `Hspec.shouldBe` (Url.baseUrl <> "films/")

    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText StarshipResource `Hspec.shouldBe` (Url.baseUrl <> "starships/")

    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText VehicleResource `Hspec.shouldBe` (Url.baseUrl <> "vehicles/")

    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText SpeciesResource `Hspec.shouldBe` (Url.baseUrl <> "species/")

    Hspec.it "is the right URL for People" $ do
      Resource.resourceToUrlText PlanetResource `Hspec.shouldBe` (Url.baseUrl <> "planets/")

-- TODO: Refactor to a prop test
spec_getId :: Spec
spec_getId =
  Hspec.describe "getId" $ do
    Hspec.it "gets ID from people resource URL" $ do
      UrlData.getId (Url.baseUrl <> "people/1/") `Hspec.shouldBe` Just 1

    Hspec.it "gets ID from film resource URL" $ do
      UrlData.getId (Url.baseUrl <> "films/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from starships resource URL" $ do
      UrlData.getId (Url.baseUrl <> "starships/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from vehicles resource URL" $ do
      UrlData.getId (Url.baseUrl <> "vehicles/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from species resource URL" $ do
      UrlData.getId (Url.baseUrl <> "species/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from planets resource URL" $ do
      UrlData.getId (Url.baseUrl <> "planets/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from URL without trailing forwardslash" $ do
      UrlData.getId (Url.baseUrl <> "planets/20") `Hspec.shouldBe` Just 20

    Hspec.it "gets Nothing from an invalid resource" $ do
      UrlData.getId (Url.baseUrl <> "peepeepoopoo/1/") `Hspec.shouldBe` Nothing

    Hspec.it "gets Nothing when there's no ID" $ do
      UrlData.getId (Url.baseUrl <> "people/") `Hspec.shouldBe` Nothing

spec_urlToUrlData :: Spec
spec_urlToUrlData =
  Hspec.describe "urlToUrlData" $ do
    Hspec.it "parses URL with params" $ do
      let
        url :: Text
        url = Url.baseUrl <> "people/?search=r2d2&page=1"

        urlParams :: Map Text Text
        urlParams = Map.fromList [ ("search", "r2d2"), ("page", "1")]

      UrlData.fromUrlText url `Hspec.shouldBe`
        Just (UrlData { udSubdir = ["people"], udParams = urlParams })

    Hspec.it "parses a URL without subdirectories or params" $ do
      let
        urlData :: UrlData
        urlData = UrlData { udSubdir = [], udParams = Map.empty }

      UrlData.fromUrlText Url.baseUrl `Hspec.shouldBe` Just urlData

    Hspec.it "parses URL without params but with subdirectories" $ do
      let
        url :: Text
        url = Url.baseUrl <> "some/subdirectory/path/with/id/1/"

        urlData :: UrlData
        urlData = UrlData
          { udSubdir = ["some", "subdirectory", "path", "with", "id", "1"]
          , udParams = Map.empty
          }

      UrlData.fromUrlText url `Hspec.shouldBe` Just urlData

    Hspec.it "parses an unexpected base URL" $ do
      UrlData.fromUrlText "https://example.com/" `Hspec.shouldBe` Nothing

spec_urlDataToUrl :: Spec
spec_urlDataToUrl =
  Hspec.describe "urlDataTourl" $ do
    Hspec.it "parses UrlData with params and subdir" $ do
      let
        urlData :: UrlData
        urlData = UrlData
          { udSubdir = ["people"]
          , udParams = Map.fromList [("search", "r2d2"), ("page", "1")]
          }

        url :: Text
        url = Url.baseUrl <> "people/?page=1&search=r2d2"

      UrlData.toUrlText urlData `Hspec.shouldBe` url

    Hspec.it "parses UrlData without params, with subdirs" $ do
      let
        urlData :: UrlData
        urlData = UrlData { udSubdir = ["people", "1"], udParams = Map.empty }

        url :: Text
        url = Url.baseUrl <> "people/1/"

      UrlData.toUrlText urlData `Hspec.shouldBe` url

    Hspec.it "parses UrlData without params, and without subdirs" $ do
      let
        urlData :: UrlData
        urlData = UrlData { udSubdir = [], udParams = Map.empty }

      UrlData.toUrlText urlData `Hspec.shouldBe` Url.baseUrl

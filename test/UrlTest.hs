module UrlTest
  ( spec_resourceUrl
  , spec_getId
  ) where

--------------------------------------------------------------------------------

import Test.Hspec (Spec)
import Test.Hspec qualified as Hspec (describe, shouldBe, it)

--------------------------------------------------------------------------------

import SwapiClient.Url
  ( Resource
      ( Root
      , People
      , Film
      , Starship
      , Vehicle
      , Species
      , Planet
      )
  )
import SwapiClient.Url qualified as Url (baseUrl, resourceUrl, getId)

--------------------------------------------------------------------------------
-- Specs

spec_resourceUrl :: Spec
spec_resourceUrl =
  Hspec.describe "resourceUrl" $ do
    Hspec.it "is the right URL for Root" $ do
      Url.resourceUrl Root `Hspec.shouldBe` Url.baseUrl

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl People `Hspec.shouldBe` (Url.baseUrl <> "people/")

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl Film `Hspec.shouldBe` (Url.baseUrl <> "films/")

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl Starship `Hspec.shouldBe` (Url.baseUrl <> "starships/")

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl Vehicle `Hspec.shouldBe` (Url.baseUrl <> "vehicles/")

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl Species `Hspec.shouldBe` (Url.baseUrl <> "species/")

    Hspec.it "is the right URL for People" $ do
      Url.resourceUrl Planet `Hspec.shouldBe` (Url.baseUrl <> "planets/")

-- TODO: Refactor to a prop test
spec_getId :: Spec
spec_getId =
  Hspec.describe "getId" $ do
    Hspec.it "gets ID from people resource URL" $ do
      Url.getId (Url.baseUrl <> "people/1/") `Hspec.shouldBe` Just 1

    Hspec.it "gets ID from film resource URL" $ do
      Url.getId (Url.baseUrl <> "films/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from starships resource URL" $ do
      Url.getId (Url.baseUrl <> "starships/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from vehicles resource URL" $ do
      Url.getId (Url.baseUrl <> "vehicles/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from species resource URL" $ do
      Url.getId (Url.baseUrl <> "species/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets ID from planets resource URL" $ do
      Url.getId (Url.baseUrl <> "planets/20/") `Hspec.shouldBe` Just 20

    Hspec.it "gets Nothing from an invalid resource" $ do
      Url.getId (Url.baseUrl <> "peepeepoopoo/1/") `Hspec.shouldBe` Nothing

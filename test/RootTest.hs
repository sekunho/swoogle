module RootTest
  ( test_toJSON
  , spec_fromJSON
  ) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (encodeFile, decodeFileStrict')
import Test.Tasty.Golden qualified as Tasty.Golden (goldenVsFile)
import Test.Tasty (TestTree)
import Test.Hspec (Spec)
import Test.Hspec qualified as Hspec (describe, shouldBe, it)

--------------------------------------------------------------------------------

import SwapiClient.Root
  ( Root
    ( Root
    , rPeople
    , rPlanets
    , rFilms
    , rSpecies
    , rVehicles
    , rStarships
    )
  )

--------------------------------------------------------------------------------

rootFixture1 :: Root
rootFixture1 = Root
  { rFilms = "https://swapi.dev/api/films/"
  , rPeople = "https://swapi.dev/api/people/"
  , rPlanets = "https://swapi.dev/api/planets/"
  , rSpecies = "https://swapi.dev/api/species/"
  , rStarships = "https://swapi.dev/api/starships/"
  , rVehicles = "https://swapi.dev/api/vehicles/"
  }

test_toJSON :: IO TestTree
test_toJSON = do
  pure $
    Tasty.Golden.goldenVsFile
      "RSJ1"
      "./testdata/root1.golden"
      "./testdata/root.json"
      (Aeson.encodeFile "./testdata/root.json" rootFixture1)

spec_fromJSON :: Spec
spec_fromJSON = do
  Hspec.describe "root schema fromJSON" $ do
    let root :: IO (Maybe Root)
        root = Aeson.decodeFileStrict' "./testdata/root.json"

    Hspec.it "parses root JSON into a Root type" $
      root >>= (\res -> res `Hspec.shouldBe` Just rootFixture1)

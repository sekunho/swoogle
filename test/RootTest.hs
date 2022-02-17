module RootTest
  ( test_toJSON
  , spec_fromJSON
  ) where

--------------------------------------------------------------------------------

import Data.Aeson                qualified as Aeson (decodeFileStrict',
                                                     encodeFile)
import Test.Hspec                (Spec)
import Test.Hspec                qualified as Hspec (describe, it, shouldBe)
import Test.Tasty                (TestTree)
import Test.Tasty.Golden         qualified as Tasty.Golden (goldenVsFile)

--------------------------------------------------------------------------------

import Swapi.Resource.Root (Root (Root, rFilms, rPeople, rPlanets, rSpecies, rStarships, rVehicles))

--------------------------------------------------------------------------------

rootFixture :: Root
rootFixture = Root
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
      "./testdata/root/root.golden"
      "./testdata/root/root.data"
      (Aeson.encodeFile "./testdata/root/root.data" rootFixture)

spec_fromJSON :: Spec
spec_fromJSON = do
  Hspec.describe "root schema fromJSON" $ do
    let root :: IO (Maybe Root)
        root = Aeson.decodeFileStrict' "./testdata/fixtures/root/root.json"

    Hspec.it "parses root JSON into a Root type" $
      root >>= (\res -> res `Hspec.shouldBe` Just rootFixture)

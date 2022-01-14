module RootTest
  ( test_toJSON
  ) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (encodeFile)
import Test.Tasty.Golden qualified as Tasty.Golden (goldenVsFile)
import Test.Tasty (TestTree)

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

test_toJSON :: IO TestTree
test_toJSON = do
  let
    sample :: Root
    sample = Root
      { rFilms = "https://swapi.dev/api/films/"
      , rPeople = "https://swapi.dev/api/people/"
      , rPlanets = "https://swapi.dev/api/planets/"
      , rSpecies = "https://swapi.dev/api/species/"
      , rStarships = "https://swapi.dev/api/starships/"
      , rVehicles = "https://swapi.dev/api/vehicles/"
      }

  pure $
    Tasty.Golden.goldenVsFile
      "RSJ1"
      "./testdata/root1.golden"
      "./testdata/root1.test"
      (Aeson.encodeFile "./testdata/root1.test" sample)

test_fromJSON :: IO TestTree
test_fromJSON = do

  pure _

module StarshipTest where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeFileStrict)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (goldenVsFile)

--------------------------------------------------------------------------------

import SwapiClient.Starship (Starship)
import SwapiClient.Page (Index)

--------------------------------------------------------------------------------

test_decodeStarshipIndex :: TestTree
test_decodeStarshipIndex =
  Golden.goldenVsFile
   "decode starship index 1"
   "./testdata/starship_index/1_decode.golden"
   "./testdata/starship_index/1_decode.data"
   (decodedIndex >>= writeFile "./testdata/starship_index/1_decode.data" . show)

  where
    decodedIndex :: IO (Either String (Index Starship))
    decodedIndex =
      Aeson.eitherDecodeFileStrict
      "./testdata/fixtures/starship_index/1.json"

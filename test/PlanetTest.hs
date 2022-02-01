module PlanetTest where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString (readFile)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import SwapiClient.Page (Index)
import SwapiClient.Resource.Planet (Planet)
import Util qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

test_decodePlanetIndex :: IO [TestTree]
test_decodePlanetIndex = mkGoldenTests <$> planetIndexJSONPaths
  where
    decodeAndWriteDestFile
      :: FilePath    -- Target file path to write stuff in
      -> ByteString  -- Contents to write in file
      -> IO ()
    decodeAndWriteDestFile destFile =
      writeFile destFile . show . Aeson.eitherDecodeStrict @(Index Planet)

    mkGoldenTests
      :: [FilePath]  -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode planet index"
        sourceFiles
        "./testdata/planet_index/decode/" -- Target directory
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= decodeAndWriteDestFile dataFile)

--------------------------------------------------------------------------------
-- Paths

planetIndexJSONPaths :: IO [FilePath]
planetIndexJSONPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/planet_index/"

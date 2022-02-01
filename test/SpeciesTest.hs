module SpeciesTest where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString (readFile)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import SwapiClient.Page (Index)
import SwapiClient.Resource.Species (SpeciesType)
import Util qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

test_decodeSpeciesIndex :: IO [TestTree]
test_decodeSpeciesIndex = mkGoldenTests <$> speciesIndexJSONPaths
  where
    decodeAndWriteDestFile
      :: FilePath    -- Target file path to write stuff in
      -> ByteString  -- Contents to write in file
      -> IO ()
    decodeAndWriteDestFile destFile =
      writeFile destFile . show . Aeson.eitherDecodeStrict @(Index SpeciesType)

    mkGoldenTests
      :: [FilePath]  -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode species index"
        sourceFiles
        "./testdata/species_index/decode/" -- Target directory
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= decodeAndWriteDestFile dataFile)

--------------------------------------------------------------------------------
-- Paths

speciesIndexJSONPaths :: IO [FilePath]
speciesIndexJSONPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/species_index/"

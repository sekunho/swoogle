module StarshipTest where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString (readFile)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import SwapiClient.Starship (Starship)
import SwapiClient.Page (Index)
import Util qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

test_decodeStarshipIndex :: IO [TestTree]
test_decodeStarshipIndex = mkGoldenTests <$> starshipIndexJSONPaths
  where
    decodeAndWriteDestFile
      :: FilePath    -- Target file path to write stuff in
      -> ByteString  -- Contents to write in file
      -> IO ()
    decodeAndWriteDestFile destFile =
      writeFile destFile . show . Aeson.eitherDecodeStrict @(Index Starship)

    mkGoldenTests
      :: [FilePath]  -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode starship index"
        sourceFiles
        "./testdata/starship_index/decode/" -- Target directory
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= decodeAndWriteDestFile dataFile)

--------------------------------------------------------------------------------
-- Paths

starshipIndexJSONPaths :: IO [FilePath]
starshipIndexJSONPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/starship_index/"

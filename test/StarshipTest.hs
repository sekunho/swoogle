module StarshipTest where

--------------------------------------------------------------------------------

import Data.Aeson                    qualified as Aeson (decodeFileStrict,
                                                         eitherDecodeStrict,
                                                         encodeFile)
import Data.ByteString               (ByteString)
import Data.ByteString               qualified as ByteString (readFile)
import Data.Maybe                    qualified as Maybe (fromJust)
import Test.Tasty                    (TestTree)
import Test.Tasty.Golden             qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import SwapiLib.Page              (Index)
import SwapiLib.Resource.Starship (Starship)
import Util                          qualified (batchGoldenVsFile)

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

test_encodeStarshipIndex :: IO [TestTree]
test_encodeStarshipIndex = mkGoldenTests <$> starshipIndexJSONPaths
  where
    encodeAndWriteDestFile
      :: FilePath -- Source file (fixture) path
      -> FilePath -- Destination file (.data) path
      -> IO ()
    encodeAndWriteDestFile sourcePath destPath =
      Aeson.decodeFileStrict @(Index Starship) sourcePath >>=
          Aeson.encodeFile destPath . Maybe.fromJust

    mkGoldenTests
      :: [FilePath] -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "encode starship index"
        sourceFiles
        "./testdata/starship_index/encode/"
        encodeAndWriteDestFile

--------------------------------------------------------------------------------
-- Paths

starshipIndexJSONPaths :: IO [FilePath]
starshipIndexJSONPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/starship_index/"

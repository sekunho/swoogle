module PersonTest
  ( test_decodePersonIndices
  , test_encodePersonIndices
  ) where

--------------------------------------------------------------------------------

import Data.Aeson                  qualified as Aeson (decodeFileStrict,
                                                       eitherDecodeStrict,
                                                       encodeFile)
import Data.ByteString             (ByteString)
import Data.ByteString.Char8       qualified as ByteString (readFile)
import Data.Maybe                  qualified as Maybe (fromJust)
import Test.Tasty                  (TestTree)
import Test.Tasty.Golden           qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import Swapi.Page            (Index)
import Swapi.Resource.Person (Person)
import Util                        qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

personIndexPaths :: IO [FilePath]
personIndexPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/person_index/"

--------------------------------------------------------------------------------

-- The reason why this exists rather than an integration test is because:
-- 1. LTE sucks
-- 2. Haven't implemented the HTTP client yet
-- 3. LTE SUCKS
test_decodePersonIndices :: IO [TestTree]
test_decodePersonIndices = mkGoldenTests <$> personIndexPaths
  where
    decodeAndWriteDestFile :: FilePath -> ByteString -> IO ()
    decodeAndWriteDestFile destFile =
        writeFile destFile . show . Aeson.eitherDecodeStrict @(Index Person)

    mkGoldenTests
      :: [FilePath]    -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode person index"
        sourceFiles
        "./testdata/person_index/decode/"
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= decodeAndWriteDestFile dataFile)

test_encodePersonIndices :: IO [TestTree]
test_encodePersonIndices =
  mkGoldenTests <$> personIndexPaths
  where
    mkGoldenTests
      :: [FilePath]
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "encode person index"
        sourceFiles
        "./testdata/person_index/encode/"
        (\sourceFile dataFile ->
           let
             personIndex :: IO (Index Person)
             personIndex = Maybe.fromJust <$> Aeson.decodeFileStrict sourceFile
           in
           personIndex >>= Aeson.encodeFile dataFile)

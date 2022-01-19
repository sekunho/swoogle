module PersonTest
  ( test_decodePersonIndices
  ) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString (readFile)
import Test.Tasty.Golden qualified as Tasty.Golden (findByExtension)
import Test.Tasty (TestTree)

--------------------------------------------------------------------------------

import SwapiClient.Person (Person)
import SwapiClient.Page (Index)
import Util qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

-- The reason why this exists rather than an integration test is because:
-- 1. LTE sucks
-- 2. Haven't implemented the HTTP client yet
-- 3. LTE SUCKS
test_decodePersonIndices :: IO [TestTree]
test_decodePersonIndices =
  mkGoldenTests <$> personIndices
  where
    personIndices :: IO [FilePath]
    personIndices =
      Tasty.Golden.findByExtension [".json"] "./testdata/fixtures/person_index/"

    writeDestFile :: FilePath -> ByteString -> IO ()
    writeDestFile destDir =
        writeFile destDir . show . Aeson.eitherDecodeStrict @(Index Person)

    mkGoldenTests
      :: [FilePath]    -- List of basenames taken from the fixture file names
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode person index"
        sourceFiles
        "./testdata/person_index/"
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= writeDestFile dataFile)

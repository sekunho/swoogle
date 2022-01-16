module PersonTest
  ( test_decodePersonIndices
  ) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeStrict)
import Data.ByteString.Char8 qualified as ByteString (pack)
import Data.Text qualified as Text (split, pack, unpack)
import Test.Tasty.Golden qualified as Tasty.Golden (goldenVsFile, findByExtension)
import Test.Tasty (TestTree)

--------------------------------------------------------------------------------

import SwapiClient.Person (PersonIndex)

--------------------------------------------------------------------------------

-- The reason why this exists rather than an integration test is because:
-- 1. LTE sucks
-- 2. Haven't implemented the HTTP client yet
-- 3. LTE SUCKS
test_decodePersonIndices :: IO [TestTree]
test_decodePersonIndices =
  map mkGoldenTest <$> personIndices
  where
    personIndices :: IO [FilePath]
    personIndices =
      Tasty.Golden.findByExtension [".json"] "./testdata/fixtures/person_index"

    mkGoldenTest :: FilePath -> TestTree
    mkGoldenTest filepath =
      let baseName :: FilePath
          baseName = takeBaseName filepath
      in
        Tasty.Golden.goldenVsFile
          ("decode page " <> baseName)
          ("./testdata/person_index/" <> baseName <> ".golden")
          ("./testdata/person_index/" <> baseName <> ".data")
          (readFile filepath >>= (
            \personIndexJSON ->
              let personIndex =
                    Aeson.eitherDecodeStrict @PersonIndex personIndexJSON
              in
                writeFile
                  ("./testdata/person_index/" <> baseName <> ".data")
                  (show personIndex)) . ByteString.pack)

takeBaseName :: FilePath -> FilePath
takeBaseName =
  Text.unpack . head . Text.split (== '.') . last . Text.split (== '/') . Text.pack

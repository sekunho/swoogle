module Util (takeBaseName, batchGoldenVsFile) where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Text qualified as Text ( unpack, split, pack )
import Test.Tasty ( TestTree )
import Test.Tasty.Golden qualified as Golden ( findByExtension, goldenVsFile )
import Test.Tasty.Providers (TestName)

--------------------------------------------------------------------------------

takeBaseName :: FilePath -> FilePath
takeBaseName =
  Text.unpack
    . head
    . Text.split (== '.')
    . last
    . Text.split (== '/')
    . Text.pack

-- | Does golden tests in batches
batchGoldenVsFile
  :: TestName             -- This will be appended with the base name of a file
  -> [FilePath]           -- List of basenames to use
  -> FilePath             -- Output path (just path, not file)
  -> (FilePath -> FilePath -> IO ())  -- Will handle writing the `.data` file
  -> [TestTree]
batchGoldenVsFile testName sourceFiles destDir arbitraryIO =
  map mkGoldenTest sourceFiles
  where
    mkGoldenTest :: FilePath -> TestTree
    mkGoldenTest sourceFile =
      let
        baseName :: FilePath
        baseName = takeBaseName sourceFile
      in
        Golden.goldenVsFile
          (testName <> ": " <> baseName)
          (destDir <> baseName <> ".golden")
          (destDir <> baseName <> ".data")
          (arbitraryIO sourceFile (destDir <> baseName <> ".data"))

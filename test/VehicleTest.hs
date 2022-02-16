module VehicleTest where

--------------------------------------------------------------------------------

import Data.Aeson                   qualified as Aeson (eitherDecodeStrict)
import Data.ByteString              (ByteString)
import Data.ByteString              qualified as ByteString (readFile)
import Test.Tasty                   (TestTree)
import Test.Tasty.Golden            qualified as Golden (findByExtension)

--------------------------------------------------------------------------------

import Swapi.Page             (Index)
import Swapi.Resource.Vehicle (Vehicle)
import Util                         qualified (batchGoldenVsFile)

--------------------------------------------------------------------------------

test_decodeVehicleIndex :: IO [TestTree]
test_decodeVehicleIndex = mkGoldenTests <$> vehicleIndexJSONPaths
  where
    decodeAndWriteDestFile
      :: FilePath    -- Target file path to write stuff in
      -> ByteString  -- Contents to write in file
      -> IO ()
    decodeAndWriteDestFile destFile =
      writeFile destFile . show . Aeson.eitherDecodeStrict @(Index Vehicle)

    mkGoldenTests
      :: [FilePath]  -- List of fixture paths
      -> [TestTree]
    mkGoldenTests sourceFiles =
      Util.batchGoldenVsFile
        "decode vehicle index"
        sourceFiles
        "./testdata/vehicle_index/decode/" -- Target directory
        (\sourceFile dataFile ->
           ByteString.readFile sourceFile >>= decodeAndWriteDestFile dataFile)

--------------------------------------------------------------------------------
-- Paths

vehicleIndexJSONPaths :: IO [FilePath]
vehicleIndexJSONPaths =
  Golden.findByExtension [".json"] "./testdata/fixtures/vehicle_index/"

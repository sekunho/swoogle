module FilmTest where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson (eitherDecodeFileStrict)
import Data.ByteString qualified as ByteString (writeFile)
import Data.ByteString.Char8 qualified as Char8 (pack)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (goldenVsFile)

--------------------------------------------------------------------------------

import SwapiClient.Film (Film)

--------------------------------------------------------------------------------

film1Fixture :: FilePath
film1Fixture = "./testdata/fixtures/film/1.json"

film1Golden :: FilePath
film1Golden = "./testdata/film/1.golden"

film1Data :: FilePath
film1Data = "./testdata/film/1.data"

test_decodeFilmJSON :: IO TestTree
test_decodeFilmJSON =
  Golden.goldenVsFile "decode film 1" film1Golden film1Data . writeDecoded
    <$> Aeson.eitherDecodeFileStrict "./testdata/fixtures/film/1.json"

  where
    writeDecoded :: Either String Film -> IO ()
    writeDecoded =
      ByteString.writeFile "./testdata/film/1.data" . Char8.pack . show

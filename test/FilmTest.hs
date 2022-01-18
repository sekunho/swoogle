module FilmTest where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
  ( eitherDecodeFileStrict
  , encodeFile
  , decodeFileStrict
  )
import Data.ByteString qualified as ByteString (writeFile)
import Data.ByteString.Char8 qualified as Char8 (pack)
import Data.Maybe qualified as Maybe (fromJust)
import Test.Tasty (TestTree)
import Test.Tasty.Golden qualified as Golden (goldenVsFile)

--------------------------------------------------------------------------------

import SwapiClient.Film
  ( Film
    ( fTitle
    , fEpisodeId
    , fOpeningCrawl
    , fDirector
    , fProducers
    , fReleaseDate
    , fCharacters
    , fPlanets
    , fStarships
    , fVehicles
    , fSpecies
    , fCreatedAt
    , fEditedAt
    , fId
    )
  )

--------------------------------------------------------------------------------
-- Paths & fixtures

film1Fixture :: FilePath
film1Fixture = "./testdata/fixtures/film/1.json"

film1DecodeGolden :: FilePath
film1DecodeGolden = "./testdata/film/1_decode.golden"

film1DecodeData :: FilePath
film1DecodeData = "./testdata/film/1_decode.data"

film1EncodeGolden :: FilePath
film1EncodeGolden = "./testdata/film/1_encode.golden"

film1EncodeData :: FilePath
film1EncodeData = "./testdata/film/1_encode.data"

film1Either :: IO (Either String Film)
film1Either = Aeson.eitherDecodeFileStrict film1Fixture

film1Maybe :: IO (Maybe Film)
film1Maybe = Aeson.decodeFileStrict film1Fixture

--------------------------------------------------------------------------------

test_decodeFilmJSON :: IO TestTree
test_decodeFilmJSON =
  Golden.goldenVsFile
    "decode film 1"
    film1DecodeGolden
    film1DecodeData
    . writeDecoded film1DecodeData
  <$> film1Either

test_encodeFilmJSON :: IO TestTree
test_encodeFilmJSON =
  Golden.goldenVsFile
    "encode film 1"
    film1EncodeGolden
    film1EncodeData
    . Aeson.encodeFile film1EncodeData
    . Maybe.fromJust
    <$> film1Maybe

--------------------------------------------------------------------------------

-- | TODO: Document this
writeDecoded
  :: (FromJSON a, Show a)
  => FilePath
  -> Either String a
  -> IO ()
writeDecoded filePath =
  ByteString.writeFile filePath . Char8.pack . show

module SwapiClient.Url
  ( Resource (Root, People, Film, Starship, Vehicle, Species, Planet)
  , UrlData (UrlData, udSubdir, udParams)
  , resourceUrl
  , getId
  , parseKeyValue
  , urlToUrlData
  , urlDataToUrl
  , baseUrl
  ) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as Text
  ( append
  , split
  , stripPrefix
  , splitOn
  , take
  , drop
  , dropWhileEnd
  , null
  , cons
  )
import Data.Text.Read qualified as Text.Read (decimal)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (fromList, foldlWithKey', null)

--------------------------------------------------------------------------------
-- Data types

data Resource
  = Root
  | People
  | Film
  | Starship
  | Vehicle
  | Species
  | Planet
  deriving Show

data UrlData = UrlData
  {
    -- | The subdirectory of a URL.
    -- e.g "/hey/1" = ["hey", "1"]
    udSubdir :: [Text]
    -- | URL parameters.
    -- e.g "hello/?key1=val1" = fromList [("key1", "val1")]
  , udParams :: Map Text Text
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Functions

baseUrl :: Text
baseUrl = "https://swapi.dev/api/"

resourceUrl :: Resource -> Text
resourceUrl resource =
  Text.append baseUrl $
    case resource of
      Root -> ""
      People -> "people/"
      Film -> "films/"
      Starship -> "starships/"
      Vehicle -> "vehicles/"
      Species -> "species/"
      Planet -> "planets/"

-- Gets the ID of the resource URL
-- > getId ("https://swapi.dev/api/people/1/" :: Text)
-- Right 1
-- TODO: Maybe make a sum type for all the ID newtypes?
getId :: Text -> Maybe Int
getId url = do
  urlData <- urlToUrlData url
  textId <- f urlData

  decimalMaybe textId

  where
    f :: UrlData -> Maybe Text
    f urlData =
      case udSubdir urlData of
        ["people", peopleId]      -> Just peopleId
        ["films", filmId]         -> Just filmId
        ["starships", starshipId] -> Just starshipId
        ["vehicles", vehicleId]   -> Just vehicleId
        ["species", speciesId]    -> Just speciesId
        ["planets", planetId]     -> Just planetId
        _                         -> Nothing

decimalMaybe :: Integral a => Text -> Maybe a
decimalMaybe t = case Text.Read.decimal t of
  Right (intId, "") -> Just intId
  Right (_, _) -> Nothing
  Left _ -> Nothing

-- I do not like this. I'm sure there's a URL library out there but this'll do.
urlToUrlData :: Text -> Maybe UrlData
urlToUrlData strippedUrl = do
  urlDataText <- Text.stripPrefix baseUrl strippedUrl

  let (subdirText:paramsText) = Text.splitOn "/?" urlDataText

      subdirs :: [Text]
      subdirs =
        if Text.null subdirText
        then []
        else Text.split (== '/') . Text.dropWhileEnd (== '/') $ subdirText

      params :: Map Text Text
      params = Map.fromList $
        map parseKeyValue $
          concatMap (Text.split (== '&')) paramsText

  pure (UrlData { udSubdir = subdirs, udParams = params })

{- TODO: Add URL encoding
   https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding -}
urlDataToUrl :: UrlData -> Text
urlDataToUrl urlData =
  mconcat
    [ baseUrl
    , subdirToUrlSubdir . udSubdir $ urlData
    , paramsToUrlParams . udParams $ urlData
    ]
  where
    subdirToUrlSubdir :: [Text] -> Text
    subdirToUrlSubdir = mconcat . reverse . foldl' (\acc el -> "/":el:acc) []

    paramsToUrlParams :: Map Text Text -> Text
    paramsToUrlParams params
      | Map.null params = mempty
      | otherwise =
        Text.cons '?' $
          Text.drop 1 $
            Map.foldlWithKey'
              (\acc key val -> acc <> "&" <> key <> "=" <> val)
              mempty
              params


-- TODO: Fix?? I am in pain.
-- Keeps accumulating as `key` until it encounters the first `=`. Once it does,
-- every succeeding character is accumulated as a `value`.
parseKeyValue :: Text -> (Text, Text)
parseKeyValue = go ("", "") False
  where
    go :: (Text, Text) -> Bool -> Text -> (Text, Text)
    go kv _ "" = kv
    go (key, value) isEncountered paramText =
      let currChar :: Text
          currChar = Text.take 1 paramText

          paramText' :: Text
          paramText' = Text.drop 1 paramText
      in
        if isEncountered
        then go (key, value <> currChar) True (Text.drop 1 paramText)
        else case currChar of
          "=" -> go (key, value) True paramText'

          _ ->
            if isEncountered
            then go (key, value <> currChar) isEncountered paramText'
            else go (key <> currChar, value) isEncountered paramText'

module SwapiClient.Url
  ( Resource (Root, People, Film, Starship, Vehicle, Species, Planet)
  , resourceUrl
  , getId
  , parseKeyValue
  , mkUrlData
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
  ( append
  , split
  , stripPrefix
  , splitOn
  , take
  , drop
  , dropWhileEnd
  )
import Data.Text.Read qualified as Text.Read (decimal)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (fromList)

--------------------------------------------------------------------------------
-- DATA TYPES

data Resource
  = Root
  | People
  | Film
  | Starship
  | Vehicle
  | Species
  | Planet
  deriving Show

-- I don't know what to name this.
data UrlData = UrlData
  { udSubdir :: [Text]
  , udParams :: Map Text Text
  }
  deriving Show

--------------------------------------------------------------------------------
-- FUNCTIONS

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
getId :: Text -> Either String Int
getId url =
  case Text.split (== '/') url of
    [_, _, _, _, _, resourceId, _] ->
      case Text.Read.decimal resourceId of
        Right (intId, "") -> Right intId
        Right _ -> Left "ERROR: Unexpected ID format"
        Left e -> Left e

    _ -> Left "ERROR: Unexpected URL format"

-- I do not like this. I'm sure there's a URL library out there but this'll do.
mkUrlData :: Text -> Maybe UrlData
mkUrlData strippedUrl = do
  urlDataText <- Text.stripPrefix "https://swapi.dev/api/" strippedUrl

  let (subdirText:paramsText) = Text.splitOn "/?" urlDataText

      subdirs :: [Text]
      subdirs = Text.split (== '/') . Text.dropWhileEnd (== '/') $ subdirText

      params :: Map Text Text
      params = Map.fromList $ map parseKeyValue $ concatMap (Text.split (== '&')) paramsText

  pure (UrlData { udSubdir = subdirs, udParams = params })

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

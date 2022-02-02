module SwapiClient.Internal.UrlData
  ( module Types
  , getId
  , decimalMaybe
  , toUrlText
  , fromUrlText
  ) where

--------------------------------------------------------------------------------

import Data.List                          qualified as List (foldl')
import Data.Map                           (Map)
import Data.Map                           qualified as Map (foldlWithKey',
                                                            fromList, null)
import Data.Text                          (Text)
import Data.Text                          qualified as Text (cons, drop,
                                                             dropWhileEnd, null,
                                                             split, splitOn,
                                                             stripPrefix, take)
import Data.Text.Read                     qualified as Text.Read (decimal)

--------------------------------------------------------------------------------

import SwapiClient.Internal.Url           qualified as Url (baseUrl)
import SwapiClient.Internal.UrlData.Types as Types (UrlData (UrlData, udParams, udSubdir))

--------------------------------------------------------------------------------

{- |
Gets the ID of the resource URL

>>> getId "https://swapi.dev/api/people/1/"
Just 1
-}
getId :: Text -> Maybe Word
getId url = do
  urlData <- fromUrlText url
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
  Right (_, _)      -> Nothing
  Left _            -> Nothing

-- I do not like this. I'm sure there's a URL library out there but this'll do.
fromUrlText :: Text -> Maybe UrlData
fromUrlText strippedUrl = do
  urlDataText <- Text.stripPrefix Url.baseUrl strippedUrl

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
toUrlText :: UrlData -> Text
toUrlText urlData =
  mconcat
    [ Url.baseUrl
    , subdirToUrlSubdir . udSubdir $ urlData
    , paramsToUrlParams . udParams $ urlData
    ]
  where
    subdirToUrlSubdir :: [Text] -> Text
    subdirToUrlSubdir = mconcat . reverse . List.foldl' (\acc el -> "/":el:acc) []

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

--------------------------------------------------------------------------------
-- Functions that shouldn't be re-exported to non-internal modules.

{- |
Keeps accumulating as @key@ until it encounters the first @=@. Once it does,
every succeeding character is accumulated as a @value@.
-}
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

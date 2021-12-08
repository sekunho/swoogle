module SwapiClient.Page
  ( Page (PersonPage, NoPage)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson.Types
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (String, Null)
  , Parser
  )

import Data.Map.Strict qualified as Map (lookup, fromList)
import Data.Kind (Type)
import Data.Text.Read qualified as Text.Read (decimal)
import TextShow qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import SwapiClient.Url (UrlData (UrlData, udParams, udSubdir))
import SwapiClient.Url qualified as Url (urlToUrlData, urlDataToUrl)

--------------------------------------------------------------------------------
-- Data types

{- FIXME: `PersonIndex` can have a page of a different resource. e.g `PersonIndex`
has `StarshipPage`, which doesn't make sense. -}
data Page
  = PersonPage Int
  | NoPage
  deriving Show

--------------------------------------------------------------------------------
-- Instances

instance FromJSON (Page :: Type) where
  parseJSON :: Value -> Parser Page
  parseJSON val =
    case val of
      String pageUrl ->
        case Url.urlToUrlData pageUrl of
          Just urlData ->
            case Map.lookup "page" (udParams urlData) of
              Just pageNum  ->
                case Text.Read.decimal pageNum of
                  Right (pageNum', "") ->
                    case udSubdir urlData of
                      ["people"] -> pure . PersonPage $ pageNum'
                      _ -> fail "ERROR: Unexpected resource from URL"
                  Right _ -> fail "ERROR: Invalid page number format."
                  Left e -> fail e
              Nothing -> pure NoPage
          Nothing -> fail "ERROR: Unable to strip base URL. It might be invalid."

      Null -> pure NoPage
      _ -> fail "ERROR: This isn't part of the API spec"

instance ToJSON (Page :: Type) where
  toJSON :: Page -> Value
  toJSON = String . maybe "null" Url.urlDataToUrl . pageToUrlData

--------------------------------------------------------------------------------
-- Functions

pageToUrlData :: Page -> Maybe UrlData
pageToUrlData =
  \case
    PersonPage pageNum ->
      Just $
        UrlData
          { udSubdir = ["people"]
          , udParams = Map.fromList [("page", Text.Show.showt pageNum)]
          }

    NoPage -> Nothing
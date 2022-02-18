module Swapi.Internal.Page.Types where

--------------------------------------------------------------------------------

import Data.Aeson                   (parseJSON, toJSON)
import Data.Aeson.Types             (FromJSON, Parser, ToJSON,
                                     Value (Null, String))
import Data.Kind                    (Type)
import Data.Map                     qualified as Map (fromList, lookup)
import Data.Text.Read               qualified as Text.Read (decimal)
import TextShow                     (TextShow)
import TextShow                     qualified as Text.Show (showt)

--------------------------------------------------------------------------------

import Swapi.Internal.UrlData       qualified as UrlData (fromUrlText,
                                                          toUrlText)
import Swapi.Internal.UrlData.Types (UrlData (UrlData, udParams, udSubdir))

--------------------------------------------------------------------------------
-- Data types

-- | Represents a page number of a resource.
newtype PageNum = MkPageNum Word
  deriving stock (Eq, Show)
  deriving newtype TextShow

{- REVIEW: I'm not sure if this should use the `Maybe` context. I think it should.
Especially since I could take advantage of `aeson`'s maybe instances and deal
with `newtype`s.
-}
{- |
`Page`s have two outcomes: `Page`, and `NoPage`. So if ever the page field
in the index JSON object is `null`, then it decodes it into `NoPage`. Otherwise,
it just puts the page number in `Page`.

__Examples:__

@
pageOne :: Page
pageOne = Page 1

blankpage :: Page
blankPage = NoPage
@
-}
data Page
  = Page Word -- ^ Presence of a page
  | NoPage   -- ^ Absence of a page
  deriving stock
    ( Eq    -- ^ Compare `Page`s with each other
    , Show  -- ^ Encode `Page` as `String` with `show`
    )

{- |
Used when decoding the index response of a particular resource. It has `count`,
`next`, `previous`, and `results`. The index is paginated. `Index` is also used
to decode search results.

The `next` and `previous` page values are further parsed into a more specific
type called `Page`.

__Example__:

@
-- An empty index. You may run into this when you're searching for something that
-- doesn't exist.
Index
  { iCount        = 0
  , iNextPage     = NoPage
  , iPreviousPage = NoPage
  , iResults      = []
  }
@
-}
-- TODO: Refactor to turn this into a sum type: HasNext, HasPrev, HasNone, HasBoth
data Index a = Index
  { iCount        :: Int  -- ^ Total number of entries for a resource
  , iNextPage     :: Page -- ^ Next page
  , iPreviousPage :: Page -- ^ Previous page
  , iResults      :: [a]  -- ^ List of resources
  }
  deriving stock Show

instance Functor Index where
  fmap f index = index {iResults = f <$> iResults index}

--------------------------------------------------------------------------------
-- Instances

-- | Decodes a JSON value to a `Page`
instance FromJSON (Page :: Type) where
  parseJSON :: Value -> Parser Page
  parseJSON val =
    case val of
      String pageUrl ->
        case UrlData.fromUrlText pageUrl of
          Just urlData ->
            case Map.lookup "page" (udParams urlData) of
              Just pageNum  ->
                case Text.Read.decimal pageNum of
                  Right (pageNum', "") ->
                    pure . Page $ pageNum'
                  Right _ -> fail "ERROR: Invalid page number format."
                  Left e -> fail e
              Nothing -> pure NoPage
          Nothing -> fail "ERROR: Unable to strip base URL. It might be invalid."

      Null -> pure NoPage
      _ -> fail "ERROR: This isn't part of the API spec"

-- | Encodes a `Page` to a JSON value
instance ToJSON (Page :: Type) where
  toJSON :: Page -> Value
  toJSON = String . maybe "null" UrlData.toUrlText . toUrlData

--------------------------------------------------------------------------------
-- Functions

toUrlData :: Page -> Maybe UrlData
toUrlData =
  \case
    Page pageNum ->
      Just $
        UrlData
          { udSubdir = ["people"]
          , udParams = Map.fromList
            [ ("page", Text.Show.showt pageNum)
            , ("format", "json")]
          }

    NoPage -> Nothing

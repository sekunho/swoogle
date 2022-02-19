module SwoogleWeb.Components.Search
  ( suggestionsEntry
  ) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid     (Html)
import Lucid qualified

--------------------------------------------------------------------------------

-- TODO: Use domain types
suggestionsEntry :: Text -> Text -> Html ()
suggestionsEntry resource query =
  Lucid.a_ [Lucid.class_ "search-suggestions-entry flex p-2.5 items-center hover:bg-su-bg/[0.2] dark:hover:bg-su-dark-bg/[0.2]", Lucid.href_ (buildHref resource query)] $ do
    Lucid.span_ [Lucid.class_ "text-su-fg dark:text-su-dark-fg"] $
      Lucid.span_ [Lucid.class_ "font-semibold"] (Lucid.toHtml query)
    Lucid.span_ [Lucid.class_ "ml-2.5 text-xs opacity-50 text-su-fg dark:text-su-dark-fg"] ("from " <> Lucid.toHtml resource)

buildHref :: Text -> Text -> Text
buildHref resource query = "/search?page=1&resource=" <> resource <> "&query=" <> query

module SwoogleWeb.Components.Search
  ( suggestionsEntry
  , searchBar
  ) where

--------------------------------------------------------------------------------

import Data.List                  (foldl')
import Data.Text                  (Text)
import Data.Text                  qualified as Text (toLower)
import Lucid                      (Html, a_, action_, autocomplete_, button_,
                                   class_, disabled_, div_, form_, hidden_,
                                   href_, id_, input_, method_, name_, option_,
                                   required_, select_, selected_, span_, type_,
                                   value_)
import Lucid                      qualified (toHtml)

--------------------------------------------------------------------------------

import Swoogle.SearchData         (SearchData (sdQuery, sdResource))
import SwoogleWeb.Components.Icon qualified as Icon (search)

--------------------------------------------------------------------------------

searchBar :: SearchData -> Html ()
searchBar searchData =
  div_ [class_ "flex flex-col sm:flex-row justify-center items-center gap-4 w-full"] $ do
    a_ [href_ "/", class_ "font-serif font-semibold text-center text-2xl sm:text-4xl text-5xl text-su-fg dark:text-su-dark-fg"]
      (span_ [class_ "text-yellow-500"] "sw" <> "oogle")

    form_
      [autocomplete_ "off", action_ "/search", method_ "GET", id_ "search", class_ "relative flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [id_ "search-bar-wrapper", class_ "w-full flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded-t rounded-b relative"] $ do
        -- This will set page to always be `1`. I need this because every new
        -- search should start from the first page. Even if it means I was at the
        -- nth page, having a new query should take me back to the top of whatever
        -- the results of the new query are.
        input_ [required_ "required", type_ "text", name_ "page", hidden_ "hidden", value_ "1"]

        input_
          [ value_ (sdQuery searchData)
          , type_ "text"
          , id_ "search-bar"
          , name_ "query"
          , class_ "rounded-l outline-none bg-inherit w-full p-2 font-sans text-su-fg dark:text-white text-base"
          , required_ ""
          ]

        -- Resource selector
        select_
          [ id_ "category-options"
          , name_ "resource"
          , class_ "font-semibold bg-white dark:bg-su-dark-bg-alt text-su-fg dark:text-su-dark-fg"
          ] $ do
          option_ [disabled_ "disabled", selected_ ""] "Category"

          foldl'
            (\opts label ->
               let
                 defaultAttrs =
                   if sdResource searchData == Text.toLower label
                   then [selected_ ""]
                   else []
               in
                 opts <>
                   option_
                     (value_ (Text.toLower label):defaultAttrs)
                     (Lucid.toHtml label))
            mempty
            ["People", "Film", "Starship", "Vehicle", "Species", "Planet"]

        button_ [type_ "submit", class_"w-2/8 px-2.5 text-su-fg dark:text-su-dark-fg rounded-r hover:bg-white/[0.1]"] $
          span_ Icon.search

      div_ [id_ "search-suggestions", class_ "z-10 border-t border-su-bg dark:border-su-dark-bg hidden bg-white dark:bg-su-dark-bg-alt absolute w-full top-full rounded-b shadow-md dark:shadow-black/[0.2]"] ""


-- TODO: Use domain types
suggestionsEntry :: Text -> Text -> Html ()
suggestionsEntry resource query =
  a_ [class_ "search-suggestions-entry flex p-2.5 items-center hover:bg-su-bg/[0.2] dark:hover:bg-su-dark-bg/[0.2]", href_ (buildHref resource query)] $ do
    span_ [class_ "text-su-fg dark:text-su-dark-fg"] $
      span_ [class_ "font-semibold"] (Lucid.toHtml query)
    span_ [class_ "ml-2.5 text-xs opacity-50 text-su-fg dark:text-su-dark-fg"] ("from " <> Lucid.toHtml resource)

buildHref :: Text -> Text -> Text
buildHref resource query = "/search?page=1&resource=" <> resource <> "&query=" <> query

module Swoogle.Views.SearchResults where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as Text (toLower)
import Lucid

import Lucid qualified as Lucid (toHtml)
import Swoogle.Components.Icon qualified as Icon (search, github)
import Swoogle.Components.Search qualified as Search (suggestionsEntry)

content :: Html ()
content = do
    div_ [class_ "w-2/3 py-6 flex flex-col min-h-screen mx-auto"] $ do
      searchBar

      div_
        [class_ "mt-10 h-1 border-b border-black/[0.2] dark:border-white/[0.1] w-full block sm:hidden"]
        ""

      -- Search results are rendered here
      div_ [class_ "mt-4 sm:mt-14 w-full flex flex-col gap-8"] $ do
        foldl' (\acc _ -> acc <> resultEntry) mempty [1..10]

      div_ [class_ "mt-4 sm:mt-6 flex justify-center gap-4"] $ do
        a_ [href_ "/search?q=luke&page=3", class_ "text-su-fg bg-gray-200 hover:bg-gray-300 dark:text-su-dark-fg dark:bg-su-dark-bg-alt dark:hover:bg-su-dark-bg-alt/[0.7] rounded-md py-2 px-4 text-sm"] "Previous"
        a_ [href_ "/search?q=luke&page=1", class_ "text-su-fg bg-gray-200 hover:bg-gray-300 dark:text-su-dark-fg dark:bg-su-dark-bg-alt dark:hover:bg-su-dark-bg-alt/[0.7] rounded-md py-2 px-4 text-sm"] "Next"

--------------------------------------------------------------------------------
-- Components

searchBar :: Html ()
searchBar =
  div_ [class_ "flex flex-col sm:flex-row justify-center items-center gap-4 w-full"] $ do
    a_ [href_ "/", class_ "font-serif text-center text-2xl sm:text-4xl text-5xl text-su-fg dark:text-su-dark-fg"]
      (span_ [class_ "text-yellow-500"] "sw" <> "oogle")

    form_
      [autocomplete_ "off", action_ "/search", method_ "GET", id_ "search", class_ "relative flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [id_ "search-bar-wrapper", class_ "w-full flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded-t rounded-b relative"] $ do
        input_ [type_ "text", min_ "1", id_ "search-bar", name_ "q", class_ "rounded-l outline-none bg-inherit w-full p-2 font-sans text-su-fg dark:text-white text-base"]
        button_ [type_ "submit", class_"w-2/8 px-2.5 text-su-fg dark:text-su-dark-fg rounded-r hover:bg-white/[0.1]"] $
          span_ Icon.search

      div_ [class_ "w-full absolute top-full"] $ do
        ul_ [class_ "overflow-x-auto mt-2.5 sm:mt-2 flex gap-4"] $ do
          mapM
            (\label -> li_ [class_ "opacity-90 text-sm text-su-fg dark:text-su-dark-fg"] $ resourceButton "anakin" label (Text.toLower label))
            ["All", "People", "Films", "Starships", "Vehicles", "Species", "Planets"]

      div_ [id_ "search-suggestions", class_ "border-t border-su-bg dark:border-su-dark-bg hidden bg-white dark:bg-su-dark-bg-alt absolute w-full h-full top-full rounded-b shadow-md dark:shadow-black/[0.2]"] $ do
        Search.suggestionsEntry "People" "luke"

resourceButton :: Text -> Text -> Text -> Html ()
resourceButton query label resource  =
  a_
    [href_ ("/search?r=" <> resource <> "&q=" <> query)]
    (Lucid.toHtml label)

resultEntry :: Html ()
resultEntry =
  div_ [class_ "flex flex-col gap-2"] $ do
    a_ [href_ "#", class_ "hover:underline text-su-fg dark:text-su-dark-fg text-xl"] "Luke Skywalker"
    p_ [class_ "text-sm text-su-fg dark:text-su-dark-fg font-light"] "This is a sample description text"
    div_ [class_ "text-xs flex gap-2 text-su-accent-1 dark:text-su-dark-fg/[0.8] opacity-80"] $ do
      span_ [class_ "px-2 py-1 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"] "180m"
      span_ [class_ "px-2 py-1 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"] "people"
      span_ [class_ "px-2 py-1 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"] "brown eyes"
      span_ [class_ "px-2 py-1 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"] "fair skin"

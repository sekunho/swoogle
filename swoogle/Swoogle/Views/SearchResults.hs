module Swoogle.Views.SearchResults where

import Data.Text (Text)
import Data.Text qualified as Text (toLower)
import Lucid

import Lucid qualified as Lucid (toHtml)
import Swoogle.Components.Icon qualified as Icon (search, github)
import Swoogle.Components.Search qualified as Search (suggestionsEntry)

content :: Html ()
content = do
    div_ [class_ "py-6 flex flex-col min-h-screen items-center"] $ do
      searchBar

      div_
        [class_ "mt-10 h-1 border-b border-black/[0.2] dark:border-white/[0.1] w-full block sm:hidden"]
        ""

      div_ [class_ "mt-4 sm:mt-12 w-full"] "Hello world!"

searchBar :: Html ()
searchBar =
  div_ [class_ "flex flex-col sm:flex-row justify-center items-center gap-4 w-full sm:w-2/3"] $ do
    h1_ [class_ "font-serif text-center text-2xl sm:text-4xl text-5xl text-su-fg dark:text-su-dark-fg"]
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
            ["People", "Films", "Starships", "Vehicles", "Species", "Planets"]

      div_ [id_ "search-suggestions", class_ "border-t border-su-bg dark:border-su-dark-bg hidden bg-white dark:bg-su-dark-bg-alt absolute w-full h-full top-full rounded-b shadow-md dark:shadow-black/[0.2]"] $ do
        Search.suggestionsEntry "People" "luke"


resourceButton :: Text -> Text -> Text -> Html ()
resourceButton query label resource  =
  a_
    [href_ ("/search?r=" <> resource <> "&q=" <> query)]
    (Lucid.toHtml label)

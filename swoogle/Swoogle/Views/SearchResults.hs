module Swoogle.Views.SearchResults where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as Text (toLower)
import Lucid

import Lucid qualified as Lucid (toHtml)
import TextShow qualified as Show (showt)

import Swoogle.Entry
import Swoogle.Components.Icon qualified as Icon (search, github)
import Swoogle.Components.Search qualified as Search (suggestionsEntry)
import Swapi

--------------------------------------------------------------------------------

content :: Text -> Index Entry -> Html ()
content partialUrl entryIndex = do
    div_ [class_ "w-full sm:w-2/3 py-6 flex flex-col min-h-screen mx-auto"] $ do
      searchBar

      div_
        [class_ "mt-10 h-1 border-b border-black/[0.2] dark:border-white/[0.1] w-full block sm:hidden"]
        ""

      -- Search results are rendered here
      div_ [class_ "mt-4 sm:mt-14 w-full flex flex-col gap-8"] $ do
        foldl' (\acc -> (<>) acc . resultEntry) mempty (iResults entryIndex)

      div_ [class_ "mt-4 sm:mt-6 flex justify-center gap-4"] $ do
        prevPage
        nextPage
  where
    prevPage =
      case iPreviousPage entryIndex of
        Page num -> navLink "Previous" (partialUrl <> "&page=" <> Show.showt num)
        NoPage -> mempty

    nextPage =
      case iNextPage entryIndex of
        Page num -> navLink "Next" (partialUrl <> "&page=" <> Show.showt num)
        NoPage -> mempty

--------------------------------------------------------------------------------
-- Components
navLink :: Text -> Text -> Html ()
navLink label href =
  a_
    [ href_ href
    , class_ "text-su-fg bg-gray-200 hover:bg-gray-300 dark:text-su-dark-fg dark:bg-su-dark-bg-alt dark:hover:bg-su-dark-bg-alt/[0.7] rounded-md py-2 px-4 text-sm"
    ]
    (Lucid.toHtml label)

searchBar :: Html ()
searchBar =
  div_ [class_ "flex flex-col sm:flex-row justify-center items-center gap-4 w-full"] $ do
    a_ [href_ "/", class_ "font-serif text-center text-2xl sm:text-4xl text-5xl text-su-fg dark:text-su-dark-fg"]
      (span_ [class_ "text-yellow-500"] "sw" <> "oogle")

    form_
      [autocomplete_ "off", action_ "/search", method_ "GET", id_ "search", class_ "relative flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [id_ "search-bar-wrapper", class_ "w-full flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded-t rounded-b relative"] $ do
        input_ [type_ "text", min_ "1", id_ "search-bar", name_ "q", class_ "rounded-l outline-none bg-inherit w-full p-2 font-sans text-su-fg dark:text-white text-base"]

        select_ [name_ "r", class_ "dark:bg-su-dark-bg-alt text-su-fg dark:text-su-dark-fg"] $ do
          option_ [disabled_ "disabled", selected_ "selected"] "Category"
          option_ [value_ "people"] "People"
          option_ [value_ "film"] "Film"
          option_ [value_ "starship"] "Starship"
          option_ [value_ "vehicle"] "Vehicle"
          option_ [value_ "species"] "Species"
          option_ [value_ "planet"] "Planet"

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

resultEntry :: Entry -> Html ()
resultEntry entry =
  div_ [class_ "flex flex-col gap-2"] $ do
    a_
      [ href_ (eLink entry)
      , class_ "hover:underline text-su-fg dark:text-su-dark-fg text-base sm:text-xl"
      ]
      (Lucid.toHtml (eTitle entry))

    -- p_ [class_ "text-sm text-su-fg dark:text-su-dark-fg font-light"] "This is a sample description text"

    div_ [class_ "text-xs space-y-1 sm:space-y-2.5 text-su-accent-1 dark:text-su-dark-fg opacity-80"] $ do
      foldl'
        (\html tagName ->
          html <>
            span_
              [class_ "inline-flex px-2 py-1 mr-1 sm:mr-2.5 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"]
              (Lucid.toHtml tagName))
        mempty
        (eTags entry)

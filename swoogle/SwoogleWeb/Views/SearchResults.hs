module SwoogleWeb.Views.SearchResults where

--------------------------------------------------------------------------------

import Data.List                    (foldl')
import Data.Text                    (Text)
import Lucid                        (Html, a_, br_, class_, div_, href_, p_,
                                     span_)

import Lucid                        qualified (toHtml)

--------------------------------------------------------------------------------

import Swoogle.Entry                (BriefEntry (beLink, beTags, beTitle),
                                     DescriptiveEntry (deDescription, deLink, deTags, deTitle),
                                     Entry (HasDescription, NoDescription))
import Swoogle.SearchData           (SearchData (sdPage))
import Swoogle.SearchData           qualified as SearchData

import SwoogleWeb.Components.Search qualified as Search (searchBar)

import Swapi                        (Index (iNextPage, iPreviousPage, iResults),
                                     Page (NoPage, Page))

--------------------------------------------------------------------------------
-- Layouts

content :: SearchData -> Index Entry -> Html ()
content searchData entryIndex = do
    div_ [class_ "w-full sm:w-2/3 py-6 flex flex-col mx-auto"] $ do
      Search.searchBar searchData

      -- Search results are rendered here
      case iResults entryIndex of
        [] ->
          div_
            [class_ "h-24 mt-4 sm:mt-14 w-full flex items-center justify-center text-su-fg/[0.8] dark:text-su-dark-fg/[0.8]"] $
            p_ [class_"text-center"] $ do
              "Huh. There's nothing here."
              br_ []
              span_ [class_ "font-light"] "I could not find anything called "
              span_ [class_ "font-bold"] "asd"
              span_ [class_ "font-light"] " in "
              span_ [class_ "font-bold"] "People"
              span_ [class_ "font-light"] "."

        _ -> do
          div_ [class_ "mt-4 sm:mt-12 w-full flex flex-col gap-8"] $
            foldl' (\acc -> (<>) acc . renderEntry) mempty (iResults entryIndex)

          div_ [class_ "mt-4 sm:mt-6 flex justify-center gap-4"] $ do
            prevPage
            nextPage
  where
    prevPage =
      case iPreviousPage entryIndex of
        Page num ->
          navLink
            "Previous"
            (SearchData.toParamsText (searchData { sdPage = num}))
        NoPage -> mempty

    nextPage =
      case iNextPage entryIndex of
        -- TODO: Implement next page using UrlData
        Page num ->
          navLink
            "Next"
            (SearchData.toParamsText (searchData { sdPage = num}))
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


resourceButton :: Text -> Text -> Text -> Html ()
resourceButton query label resource  =
  a_
    [href_ ("/search?r=" <> resource <> "&q=" <> query)]
    (Lucid.toHtml label)

renderEntry :: Entry -> Html ()
renderEntry entry =
  case entry of
    HasDescription de -> buildDescriptiveEntry de
    NoDescription be  -> buildBriefEntry be

-- TODO: I should do something about this. Maybe this can just be one?
buildDescriptiveEntry :: DescriptiveEntry -> Html ()
buildDescriptiveEntry entry =
  div_ [class_ "flex flex-col gap-2"] $ do
    a_
      [ href_ (deLink entry)
      , class_ "hover:underline font-semibold text-su-fg dark:text-su-dark-fg text-base sm:text-xl"
      ]
      (Lucid.toHtml (deTitle entry))

    p_
      [class_ "text-sm text-su-fg dark:text-su-dark-fg font-light truncate"]
      (Lucid.toHtml (deDescription entry))

    div_ [class_ "text-xs space-y-1 sm:space-y-2.5 text-su-accent-1 dark:text-su-dark-fg opacity-80"] $ do
      foldl'
        (\html tagName ->
          html <>
            span_
              [class_ "inline-flex font-medium px-2 py-1 mr-1 sm:mr-2.5 rounded bg-su-accent-1/[0.2] dark:bg-su-dark-bg-alt/[0.5]"]
              (Lucid.toHtml tagName))
        mempty
        (deTags entry)

-- TODO: I should do something about this. Maybe this can just be one?
buildBriefEntry :: BriefEntry -> Html ()
buildBriefEntry entry =
  div_ [class_ "flex flex-col gap-2"] $ do
    a_
      [ href_ (beLink entry)
      , class_ "hover:underline font-semibold text-su-fg dark:text-su-dark-fg text-base sm:text-xl"
      ]
      (Lucid.toHtml (beTitle entry))

    div_ [class_ "text-xs space-y-1 sm:space-y-2.5 text-su-accent-1 dark:text-su-dark-fg opacity-80"] $ do
      foldl'
        (\html tagName ->
          html <>
            span_
              [class_ "inline-flex font-medium px-2 py-1 mr-1 sm:mr-2.5 rounded bg-su-accent-1/[0.1] dark:bg-su-dark-bg-alt/[0.5]"]
              (Lucid.toHtml tagName))
        mempty
        (beTags entry)

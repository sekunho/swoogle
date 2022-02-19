module SwoogleWeb.Views.Home where

--------------------------------------------------------------------------------

import Lucid                      (Html, a_, action_, autocomplete_, autofocus_,
                                   button_, class_, disabled_, div_, form_, h1_,
                                   hidden_, href_, id_, input_, method_, name_,
                                   option_, required_, select_, selected_,
                                   span_, target_, type_, value_)

--------------------------------------------------------------------------------

import SwoogleWeb.Components.Icon qualified as Icon (search)

--------------------------------------------------------------------------------

content :: Html ()
content = do
  div_ [class_ "flex-1 flex flex-col relative items-center justify-center"] $ do
    h1_
      [class_ "font-serif font-semibold text-center text-5xl sm:text-6xl text-7xl text-su-fg dark:text-su-dark-fg mb-12"]
      (a_ [href_ "/", class_ ""] (span_ [class_ "text-yellow-600 dark:text-yellow-500"] "sw" <> "oogle"))

    -- TODO: Move this to a component
    form_
      [autocomplete_ "off", action_ "/search", method_ "GET", id_ "search", class_ "flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [id_ "search-bar-wrapper", class_ "w-full sm:w-2/3 flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded-t rounded-b relative"] $ do
        -- Hidden page value
        input_ [required_ "required", type_ "text", name_ "page", hidden_ "hidden", value_ "1"]

        -- Search query
        input_
          [ required_ "required"
          , type_ "text"
          , id_ "search-bar"
          , name_ "query"
          , class_ "placeholder-light-italic rounded-l outline-none bg-inherit w-6/8 w-full p-2.5 font-sans text-su-fg dark:text-white text-lg"
          , autofocus_
--          , placeholder_ "What do you want to look for?"
          ]

        -- Category options
        select_
          [ id_ "category-options"
          , name_ "resource"
          , class_ "bg-white font-semibold dark:bg-su-dark-bg-alt text-su-fg dark:text-su-dark-fg"
          , required_ "required"
          ] $ do
          option_ [disabled_ "disabled", selected_ "selected", value_ ""] "Category"
          option_ [value_ "people"] "People"
          option_ [value_ "film"] "Film"
          option_ [value_ "starship"] "Starship"
          option_ [value_ "vehicle"] "Vehicle"
          option_ [value_ "species"] "Species"
          option_ [value_ "planet"] "Planet"

        -- Search button
        button_ [type_ "submit", class_"w-2/8 px-2.5 text-su-fg dark:text-su-dark-fg rounded-r hover:bg-white/[0.1]"] $
          span_ Icon.search

        div_ [id_ "search-suggestions", class_ "z-10 flex flex-col border-t border-su-bg dark:border-su-dark-bg hidden bg-white dark:bg-su-dark-bg-alt absolute w-full top-full rounded-b shadow-md dark:shadow-black/[0.2]"] ""

      div_ [class_ "text-su-fg dark:text-su-dark-fg flex flex-col sm:flex-row gap-2 text-sm sm:text-base"] $ do
        span_ [class_ "text-center sm:text-left font-light"] "A Star Wars search engine. "
        span_ [class_ "text-center sm:text-left font-semibold"] (a_ [class_ "text-yellow-600 dark:text-yellow-500 hover:text-yellow-500", href_ "https://ko-fi.com/sekun", target_ "blank"] "Support me on Kofi")

    div_ [class_ "absolute bottom-0 pb-8"] ""

middot :: Html ()
middot = span_ [class_ "hidden sm:block"] "·"


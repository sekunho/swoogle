module Swoogle.Views.Home where

import Lucid
    ( Html,
      a_,
      action_,
      button_,
      class_,
      div_,
      form_,
      h1_,
      h2_,
      href_,
      id_,
      input_,
      span_,
      target_,
      type_,
      method_,
      name_,
      autofocus_,
      autocomplete_,
      required_,
      value_,
      hidden_,
      ToHtml(toHtml, toHtmlRaw) )

import Swoogle.Components.Icon qualified as Icon (search, github)
import Swoogle.Components.Search qualified as Search (suggestionsEntry)

content :: Html ()
content = do
  div_ [class_ "flex flex-col relative items-center justify-center h-screen"] $ do
    h1_
      [class_ "font-serif text-center text-5xl sm:text-6xl text-7xl text-su-fg dark:text-su-dark-fg mb-12"]
      (span_ [class_ "text-yellow-500"] "sw" <> "oogle")

    form_
      [autocomplete_ "off", action_ "/search", method_ "GET", id_ "search", class_ "flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [id_ "search-bar-wrapper", class_ "w-full sm:w-2/3 flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded-t rounded-b relative"] $ do
        input_ [required_ "required", type_ "text", name_ "page", hidden_ "hidden", value_ "1"]
        input_ [required_ "required", type_ "text", name_ "r", hidden_ "hidden", value_ "all"]
        input_ [required_ "required", type_ "text", id_ "search-bar", name_ "q", class_ "rounded-l outline-none bg-inherit w-6/8 w-full p-2.5 font-sans text-su-fg dark:text-white text-lg", autofocus_]
        button_ [type_ "submit", class_"w-2/8 px-2.5 text-su-fg dark:text-su-dark-fg rounded-r hover:bg-white/[0.1]"] $
          span_ Icon.search

        div_ [id_ "search-suggestions", class_ "border-t border-su-bg dark:border-su-dark-bg hidden bg-white dark:bg-su-dark-bg-alt absolute w-full h-full top-full rounded-b shadow-md dark:shadow-black/[0.2]"] $ do
          Search.suggestionsEntry "People" "luke"

      div_ [class_ "text-su-fg dark:text-su-dark-fg flex gap-2 text-sm sm:text-base"] $ do
        span_ [class_ "font-light"] "A Star Wars search engine"
        middot
        span_ [] (a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://ko-fi.com/sekun", target_ "blank"] "Support me on Kofi")
        middot
        span_ [] "Made by " <> a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://twitter.com/hsekun", target_ "_blank"] "Sek Un"
        middot
        span_ [] (a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://github.com/sekunho/swapi", target_ "blank"] Icon.github)

    div_ [class_ "absolute bottom-0 pb-8"] ""

middot :: Html ()
middot = span_ [] "·"

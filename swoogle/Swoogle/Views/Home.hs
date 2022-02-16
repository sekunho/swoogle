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
      ToHtml(toHtml, toHtmlRaw) )

import Swoogle.Components.Icon qualified as Icon (search, github)

content :: Html ()
content = do
  div_ [class_ "flex flex-col items-center justify-center h-screen"] $ do
    h1_
      [class_ "font-serif text-center text-5xl sm:text-6xl text-7xl text-su-fg dark:text-su-dark-fg mb-12"]
      (span_ [class_ "text-yellow-500"] "sw" <> "oogle")

    form_
      [action_ "/search", method_ "GET", id_ "search", class_ "flex flex-col gap-8 items-center justify-center w-full"] $ do
      div_ [class_ "w-full sm:w-2/3 flex shadow-md dark:shadow-black/[0.2] bg-white dark:bg-su-dark-bg-alt rounded"] $ do
        input_ [type_ "text", id_ "query", name_ "q", class_ "rounded-l outline-none bg-inherit w-6/8 w-full p-2.5 font-sans text-su-fg dark:text-white text-lg", autofocus_]
        button_ [type_ "submit", class_"w-2/8 px-2.5 text-su-fg dark:text-su-dark-fg rounded-r hover:bg-white/[0.1]"] $
          span_ Icon.search

      div_ [class_ "text-su-fg dark:text-su-dark-fg flex gap-2 text-sm sm:text-base"] $ do
        span_ [] (a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://ko-fi.com/sekun", target_ "blank"] "Support me on Kofi")
        middot
        span_ [] "Made by " <> a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://twitter.com/hsekun", target_ "_blank"] "Sek Un"
        middot
        span_ [] (a_ [class_ "text-yellow-500 hover:text-yellow-300", href_ "https://github.com/sekunho/swapi", target_ "blank"] Icon.github)

middot :: Html ()
middot = span_ [] "·"

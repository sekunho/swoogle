-- | Contains the default layouts. I guess the outer styles or something idk how
-- to explain this lol.
module Swoogle.Views.Layout where

--------------------------------------------------------------------------------

import Data.List               (foldl')
import Data.Text               (Text)
import Lucid                   (Html, a_, async_, body_, charset_, class_,
                                content_, crossorigin_, defer_, div_,
                                doctypehtml_, footer_, href_, httpEquiv_, link_,
                                main_, meta_, name_, p_, rel_, script_, src_,
                                target_, title_)

import Lucid                   qualified (toHtmlRaw)
import Lucid.Base              (makeAttribute)

--------------------------------------------------------------------------------

import Swoogle.Components.Icon qualified as Icon (github, search, twitter,
                                                  youtube)

--------------------------------------------------------------------------------

noFooterRoot :: Html () -> Html ()
noFooterRoot content = doctypehtml_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
  meta_
    [ name_ "viewport"
    , content_ "width=device-width"
    , makeAttribute "initial-scale" "1.0"
    ]
  link_
    [ rel_ "stylesheet"
    , href_ "/assets/app.css"
    ]

  -- Fonts
  link_
    [ rel_ "preconnect"
    , href_ "https://fonts.googleapis.com"
    ]

  link_
    [ rel_ "preconnect"
    , href_ "https://fonts.gstatic.com"
    , crossorigin_ ""]

  link_
    [ href_ "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Vollkorn:wght@400;500;600;700;800&display=swap"
    , rel_ "stylesheet"]

  favicon

  title_ "swoogle: A Star Wars search engine"

  body_ [class_ "bg-gray-100 dark:bg-su-dark-bg relative min-h-screen flex flex-col"] $
    main_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 w-full flex flex-col flex-1"] $
      div_ [class_ "w-full max-w-4xl mx-auto flex-1 flex flex-col"] content

root :: Html () -> Html ()
root content = doctypehtml_ $ do
  meta_ [makeAttribute "charset" "utf-8"]
  meta_ [makeAttribute "http-equiv" "X-UA-Compatible", makeAttribute "content" "IE=edge"]
  meta_
    [ makeAttribute "name" "viewport"
    , makeAttribute "content" "width=device-width"
    , makeAttribute "initial-scale" "1.0"
    ]
  link_
    [ makeAttribute "rel" "stylesheet"
    , makeAttribute "href" "/assets/app.css"
    ]

  -- Fonts
  link_
    [ rel_ "preconnect"
    , href_ "https://fonts.googleapis.com"
    ]

  link_
    [ rel_ "preconnect"
    , href_ "https://fonts.gstatic.com"
    , crossorigin_ ""]

  link_
    [ href_ "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Vollkorn:wght@400;500;600;700;800&display=swap"
    , rel_ "stylesheet"]

  favicon

  title_ "swoogle: A Star Wars search engine"

  body_ [class_ "bg-gray-100 dark:bg-su-dark-bg relative min-h-screen flex flex-col"] $ do
    main_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 w-full flex flex-col flex-1"] $
      div_ [class_ "w-full max-w-4xl mx-auto flex-1 flex flex-col"] content

    footer

    script_ [src_ "/assets/app.js"] ("" :: Html ())
    script_
      [ async_ ""
      , defer_ ""
      , src_ "https://buttons.github.io/buttons.js"
      ]
      ("" :: Html ())

footer :: Html ()
footer =
  footer_ [class_ "bg-gray-100 dark:bg-su-dark-bg"] $ do
    div_ [class_ "max-w-7xl mx-auto pb-6 pt-2 px-4 sm:px-6 md:flex md:items-center md:justify-between lg:px-8"] $ do
      div_ [class_ "flex justify-center space-x-6 md:order-2"] $ do
        Lucid.toHtmlRaw @Text
          "<a class=\"github-button\" href=\"https://github.com/sekunho/swoogle\" data-color-scheme=\"no-preference: light; light: light; dark: dark_high_contrast;\" data-icon=\"octicon-star\" data-size=\"large\" data-show-count=\"true\" aria-label=\"Star sekunho/swoogle on GitHub\">Star</a>"

        foldl'
          (\links (label, link) ->
             links <>
               a_
                 [ href_ link
                 , target_ "_blank"
                 , class_ "text-su-fg dark:text-su-dark-fg opacity-70 hover:text-gray-500"
                 ]
                 label
          )
          mempty
          [ (Icon.github, "https://github.com/sekunho")
          , (Icon.twitter, "https://twitter.com/hsekun")
          , (Icon.youtube, "https://youtube.com/sekunho")
          ]

      div_ [class_ "mt-8 md:mt-0 md:order-1"] $ do
        p_
          [class_ "text-center text-base text-su-fg dark:text-su-dark-fg opacity-70"] $ do
          Lucid.toHtmlRaw @Text "&copy; 2022 "
          a_
            [ href_ "https://sekun.dev"
            , target_ "_blank"
            , class_ "hover:text-gray-500 underline"
            ]
            "Sek Un"

favicon :: Html ()
favicon =
  Lucid.toHtmlRaw $
    mconcat @Text
      [ "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/images/apple-touch-icon.png\">"
      , "<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/images/favicon-32x32.png\">"
      , "<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/images/favicon-16x16.png\">"
      , "<link rel=\"manifest\" href=\"/assets/site.webmanifest\">"
      ]

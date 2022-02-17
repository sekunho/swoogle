-- | Contains the default layouts. I guess the outer styles or something idk how
-- to explain this lol.
module Swoogle.Views.Layout where

-- TODO: Explicit imports
import Lucid
import Lucid.Base (makeAttribute)

root :: Monad m => HtmlT m a -> HtmlT m ()
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

  title_ "swoogle: A Star Wars search engine"

  body_ [class_ "bg-gray-100 dark:bg-su-dark-bg relative"] $ do
    main_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
      div_ [class_ "max-w-4xl mx-auto"] content
      span_
        [class_ "absolute bottom-2.5 right-7 text-su-fg dark:text-su-dark-fg/[0.5] font-light"]
        "v0.1"

    script_ [src_ "/assets/app.js"] ("" :: Html ())

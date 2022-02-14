-- | Contains the default layouts. I guess the outer styles or something idk how
-- to explain this lol.
module SwapiWeb.Views.Layout where

-- TODO: Explicit imports
import Lucid
import Lucid.Base (makeAttribute)

root :: Monad m => HtmlT m a -> HtmlT m a
root content = doctypehtml_ $ do
  meta_ [makeAttribute "charset" "utf-8"]
  meta_ [makeAttribute "http-equiv" "X-UA-Compatible", makeAttribute "content" "IE=edge"]
  meta_
    [ makeAttribute "name" "viewport"
    , makeAttribute "content" "width=device-width"
    , makeAttribute "initial-scale" "1.0"
    ]
  content

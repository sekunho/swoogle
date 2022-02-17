module Swoogle.Views.Errors where

import Data.Text (Text)
import Lucid
import Lucid qualified (toHtml)

renderError500 :: Html () -> Html ()
renderError500 slot = do
  div_ [class_ "relative h-screen w-full sm:w-2/3 flex flex-col gap-4 items-center justify-center mx-auto"] $ do
    h1_ [class_ "text-4xl text-su-fg dark:text-su-dark-fg font-serif text-center"] "Internal Server Error"
    slot

unexpectedResource :: Text -> Html ()
unexpectedResource resource = do
  renderError500 $ do
    div_ [class_ "text-su-fg dark:text-su-dark-fg text-xl"] $ do
        span_ [class_ "font-light"] "I did not expect a resource of "
        span_ [class_ "font-bold"] (Lucid.toHtml resource)
        span_ [class_ "font-light"] ". "
        span_ [] $ do
          span_ [class_ "font-light"] "If this is supposed to exist, please open an issue here: "
          a_
            [ href_ "https://github.com/sekunho/swoogle/issues"
            , target_ "_blank"
            , class_ "text-yellow-500"
            ]
            "https://github.com/sekunho/swoogle/issues"

    renderQuote
      "Be careful not to choke on your aspirations."
      "Darth Vader, Rogue One, 2016"

renderQuote :: Text -> Text -> Html ()
renderQuote quote source =
     span_ [class_ "absolute bottom-7 text-lg text-su-fg/[0.7] dark:text-su-dark-fg/[0.8]"] $ do
      "“" <> Lucid.toHtml quote <> "”"
      span_ [class_ "font-light"] (" – " <> Lucid.toHtml source <> ".")

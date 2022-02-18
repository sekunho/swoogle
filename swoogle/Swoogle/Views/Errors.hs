module Swoogle.Views.Errors where

import Data.List (foldl')
import Data.Text (Text)
import Lucid
import Lucid qualified (toHtml)
import TextShow qualified as Show (showt)

import Swoogle.SearchData
import Swoogle.SearchData qualified as SearchData (toParamsText)

renderError500 :: Html () -> Html ()
renderError500 slot = do
  div_ [class_ "text-su-fg dark:text-su-dark-fg relative h-screen w-full sm:w-2/3 flex flex-col gap-4 items-center justify-center mx-auto"] $ do
    h1_ [class_ "text-4xl font-serif text-center"] "Internal Server Error"
    slot

httpNotFound :: Html ()
httpNotFound = do
  renderError500 $ do
    div_ [class_ "text-xl"] $ do
      span_ [class_ "font-light"] "I couldn't find what you're looking for."

    renderQuote
      "These aren't the droids you're looking for."
      "Obi-Wan Kenobi, A New Hope, 1977"

unexpectedResource :: SearchData -> Html ()
unexpectedResource searchData = renderError500 $ do
  div_ [class_ "text-center"] $ do
      span_ [class_ "font-light"] "I didn't expect a resource called "
      span_ [class_ "font-bold"] (Lucid.toHtml $ sdResource searchData)
      span_ [class_ "font-light"] ". "

      case sdResource searchData of
        "films" -> renderSuggestion (searchData {sdResource = "film"})
        "peoples" -> renderSuggestion (searchData {sdResource = "people"})
        "planets" -> renderSuggestion (searchData {sdResource = "planet"})
        "specie" -> renderSuggestion (searchData {sdResource = "species"})
        "vehicles" -> renderSuggestion (searchData {sdResource = "vehicle"})
        "starships" -> renderSuggestion (searchData {sdResource = "starship"})

        _ -> ""

      span_ [class_ ""] $ do
        span_
          [class_ "font-light"]
          "Here are the resources that I know of: "
        span_ [class_ "space-x-2.5"] $ do
          foldl'
            (\items resource ->
               items <> renderResource (searchData { sdResource = resource }))
            mempty
            ["people", "film", "starship", "vehicle", "species", "planet"]

  renderQuote
    "Be careful not to choke on your aspirations."
    "Darth Vader, Rogue One, 2016"

  where
    renderResource :: SearchData -> Html ()
    renderResource searchData =
      a_
        [ class_ "font-bold text-yellow-600 dark:text-yellow-500"
        , href_ ("search" <> SearchData.toParamsText searchData)
        ]
        (Lucid.toHtml $ sdResource searchData)

    renderSuggestion :: SearchData -> Html ()
    renderSuggestion searchData =
      mconcat
        [ span_ [class_ "font-light"] "Perhaps you meant "
        , renderResource searchData
        , "? "
        , br_ []
        , br_ []
        ]

renderQuote :: Text -> Text -> Html ()
renderQuote quote source = do
  div_ [class_ "absolute bottom-7"] $ do
     p_ [class_ "font-semibold text-lg text-su-fg/[0.7] dark:text-su-dark-fg/[0.8]"] $ do
      "“" <> Lucid.toHtml quote <> "”"

     span_ [class_ "font-light"] (" – " <> Lucid.toHtml source <> ".")

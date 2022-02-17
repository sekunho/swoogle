module Main where

import Control.Monad.IO.Class qualified as IO (liftIO)
import Data.Text (Text)
import Lucid
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty
  ( scotty
  , html
  , get
  , param
  , file
  , setHeader
  , liftAndCatchIO
  , rescue
  )

import Swoogle.Entry
import Swoogle.Views.Home qualified as Home (content)
import Swoogle.Views.Layout qualified as Layout (root)
import Swoogle.Views.SearchResults qualified as Results (content)
import Swapi

main :: IO ()
main = Scotty.scotty 3000 swapiWeb

swapiWeb :: ScottyM ()
swapiWeb = do
  -- Assets
  let staticPath = "./priv/static/"

  Scotty.get
    "/assets/app.css"
    (Scotty.setHeader "Content-Type" "text/css; charset=utf-8" >>
      Scotty.file (staticPath <> "assets/app.css"))

  Scotty.get
    "/assets/app.js"
    (Scotty.setHeader "Content-Type" "application/javascript" >>
      Scotty.file (staticPath <> "assets/app.js"))

  Scotty.get "/fonts/:file" (Scotty.param "file" >>= Scotty.file . (<>) (staticPath <> "fonts/"))

  Scotty.get "/images/:file" (Scotty.param "file" >>= Scotty.file . (<>) (staticPath <> "images/"))

  -- Page routes

  Scotty.get "/" (Scotty.html (Lucid.renderText (Layout.root Home.content)))

  Scotty.get "/search" $ do
    searchQuery    <- Scotty.param @Text "q"
    searchResource <- Scotty.param @Text "r"
    searchPage     <- Scotty.param @Int "page"
    results        <- Scotty.liftAndCatchIO $ searchPeople searchQuery (Page searchPage)

    IO.liftIO $ print results

    let partialUrl :: Text
        partialUrl =
          "/search?q=" <> searchQuery <> "&r=" <> searchResource

        content :: Html ()
        content = Layout.root (Results.content partialUrl (personToEntry <$> results))


    Scotty.html (Lucid.renderText content)

--------------------------------------------------------------------------------
-- "Controllers"


searchController =
  getPerson (PersonId 1)

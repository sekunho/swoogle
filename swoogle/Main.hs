module Main where

import Lucid
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty (scotty, html, get, param, file, setHeader)

import Swoogle.Views.Home qualified as Home (content)
import Swoogle.Views.Layout qualified as Layout (root)
import Swoogle.Views.SearchResults qualified as Results (content)

main :: IO ()
main = Scotty.scotty 3000 swapiWeb

swapiWeb :: ScottyM ()
swapiWeb = do
  -- Assets

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

  Scotty.get "/search" (Scotty.html (Lucid.renderText (Layout.root Results.content)))


  where staticPath = "./priv/static/"

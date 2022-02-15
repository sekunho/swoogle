module Main where

import Lucid
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty (scotty, html, get, param, file)

import Swoogle.Views.Home qualified as Home (content)
import Swoogle.Views.Layout qualified as Layout (root)

main :: IO ()
main = Scotty.scotty 3000 swapiWeb

swapiWeb :: ScottyM ()
swapiWeb = do
  Scotty.get "/assets/:file" (Scotty.param "file" >>= Scotty.file . (<>) (staticPath <> "assets/"))

  Scotty.get "/fonts/:file" (Scotty.param "file" >>= Scotty.file . (<>) (staticPath <> "fonts/"))

  Scotty.get "/images/:file" (Scotty.param "file" >>= Scotty.file . (<>) (staticPath <> "images/"))

  Scotty.get "/" (Scotty.html (Lucid.renderText (Layout.root Home.content)))

  Scotty.get "/people" (Scotty.html (Lucid.renderText (Layout.root ("123 < 456" :: Html ()))))

  where staticPath = "swoogle/priv/static/"

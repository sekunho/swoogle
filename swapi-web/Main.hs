module Main where

import Lucid
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty (scotty, html, get, param, file)

import SwapiWeb.Views.Layout qualified as Layout (root)

main :: IO ()
main = Scotty.scotty 3000 swapiWeb

swapiWeb :: ScottyM ()
swapiWeb = do
  let staticPath = "swapi-web/priv/static/assets/"
  Scotty.get "/assets/:file" (Scotty.param "file" >>= Scotty.file . (<>) staticPath)

  Scotty.get "/people" (Scotty.html (Lucid.renderText (Layout.root ("123 < 456" :: Html ()))))

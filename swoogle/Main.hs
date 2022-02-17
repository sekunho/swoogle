module Main where

import Control.Monad.IO.Class qualified as IO (liftIO)
import Data.Map qualified as Map
import Data.Text (Text)
import Lucid
import Web.Scotty (ScottyM, ActionM)
import Web.Scotty qualified as Scotty
  ( scotty
  , html
  , get
  , param
  , file
  , setHeader
  , liftAndCatchIO
  )
import TextShow qualified as Show (showt)

import Swoogle.SearchData
import Swoogle.Entry qualified as Entry

-- Views
import Swoogle.Views.Layout qualified as Layout (root)
import Swoogle.Views.SearchResults qualified as Results (content)

-- Controllers
import Swoogle.Controllers qualified as Controllers

main :: IO ()
main = Scotty.scotty 3000 swapiWeb

swapiWeb :: ScottyM ()
swapiWeb = do
  -- Assets
  Scotty.get "/assets/:file" (Scotty.param "file" >>= Controllers.assets)
  Scotty.get "/fonts/:file" (Scotty.param "file" >>= Controllers.fonts)
  Scotty.get "/images/:file" (Scotty.param "file" >>= Controllers.images)

  -- Page routes
  Scotty.get "/" Controllers.home
  Scotty.get "/search" Controllers.search

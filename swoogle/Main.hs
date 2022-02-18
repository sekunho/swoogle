module Main where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class qualified as IO (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Map qualified as Map
import Data.Word8 qualified as Word8 (_semicolon)
import Data.Set qualified as Set
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
  , middleware
  )
import TextShow qualified as Show (showt)
import Network.Wai.Middleware.RequestLogger qualified as Logger (logStdout)
import Network.Wai.Middleware.Gzip
    ( GzipFiles(GzipCompress), GzipSettings(gzipFiles) )
import Network.Wai.Middleware.Gzip qualified as Gzip (def, gzip)

--------------------------------------------------------------------------------

-- Views
import Swoogle.Views.Layout qualified as Layout (root)
import Swoogle.Views.SearchResults qualified as Results (content)

-- Controllers
import Swoogle.Controllers qualified as Controllers

--------------------------------------------------------------------------------

main :: IO ()
main =
  Scotty.scotty
    3000
    (Scotty.middleware Logger.logStdout
       >> Scotty.middleware (Gzip.gzip gzipSettings)
       >> swapiWeb
    )
  where
    gzipSettings :: GzipSettings
    gzipSettings = Gzip.def { gzipFiles = GzipCompress }

swapiWeb :: ScottyM ()
swapiWeb = do
  -- Asset routes
  Scotty.get "/assets/:file" (Scotty.param "file" >>= Controllers.assets)
  Scotty.get "/images/:file" (Scotty.param "file" >>= Controllers.images)

  -- Page routes
  Scotty.get "/" Controllers.home
  Scotty.get "/search" Controllers.search

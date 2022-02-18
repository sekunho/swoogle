module Main where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class               qualified as IO (liftIO)
import Data.Text                            (Text)
import Network.Wai.Middleware.Gzip          (GzipFiles (GzipCompress),
                                             GzipSettings (gzipFiles))
import Network.Wai.Middleware.Gzip          qualified as Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger qualified as Logger (logStdout)
import TextShow                             qualified as Show (showt)
import Web.Scotty                           (ActionM, ScottyM)
import Web.Scotty                           qualified as Scotty (file, get,
                                                                 html,
                                                                 liftAndCatchIO,
                                                                 middleware,
                                                                 param, scotty,
                                                                 setHeader)

--------------------------------------------------------------------------------

-- Views
import Swoogle.Views.Layout                 qualified as Layout (root)
import Swoogle.Views.SearchResults          qualified as Results (content)

-- Controllers
import Swoogle.Controllers                  qualified as Controllers

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

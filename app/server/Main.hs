module Main where

--------------------------------------------------------------------------------

import Network.Wai.Middleware.Gzip          (GzipFiles (GzipCompress),
                                             GzipSettings (gzipFiles))
import Network.Wai.Middleware.Gzip          qualified as Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger qualified as Logger (logStdout)
import Web.Scotty                           qualified as Scotty (middleware,
                                                                 scotty)
--------------------------------------------------------------------------------

import SwoogleWeb.Server                    qualified as Server (routes)

--------------------------------------------------------------------------------

main :: IO ()
main =
  Scotty.scotty 3000 $
    Scotty.middleware Logger.logStdout >>
      Scotty.middleware (Gzip.gzip gzipSettings) >>
      Server.routes
  where
    gzipSettings :: GzipSettings
    gzipSettings = Gzip.def { gzipFiles = GzipCompress }


module SwoogleWeb.Server where

--------------------------------------------------------------------------------

import SwoogleWeb.Controllers qualified as Controllers
import Web.Scotty             (ScottyM)
import Web.Scotty             qualified as Scotty (get, param)

--------------------------------------------------------------------------------

routes :: ScottyM ()
routes = do
  -- Asset routes
  Scotty.get "/assets/:file" (Scotty.param "file" >>= Controllers.assets)
  Scotty.get "/images/:file" (Scotty.param "file" >>= Controllers.images)

  -- Page routes
  Scotty.get "/" Controllers.home
  Scotty.get "/search" Controllers.search
  Scotty.get "/people/:id" Controllers.showPerson

  -- Non-HTML routes
  Scotty.get "/suggest" Controllers.suggest

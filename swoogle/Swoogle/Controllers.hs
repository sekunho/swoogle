-- | Oh look at Sekun! Trying to shoehorn MVC into a non-MVC library.
module Swoogle.Controllers where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid
import Lucid qualified (renderText)
import Web.Scotty (ActionM)
import Web.Scotty qualified as Scotty
  ( liftAndCatchIO
  , html
  , raise
  , rescue
  , param
  , setHeader
  , file
  )

--------------------------------------------------------------------------------

import Swoogle.SearchData
import Swoogle.Entry (Entry)
import Swoogle.Entry qualified as Entry

-- Swoogle's views
import Swoogle.Views.Home qualified as Home (content)
import Swoogle.Views.Layout qualified as Layout (root)
import Swoogle.Views.SearchResults qualified as Results (content)
import Swoogle.Views.Errors qualified as Errors

-- Swapi
import Swapi

--------------------------------------------------------------------------------

assets :: Text -> ActionM ()
assets asset =
  -- TODO: Handle other cases
  case asset of
    "app.css" ->
      Scotty.setHeader "Content-Type" "text/css; charset=utf-8" >>
        Scotty.file (staticPath <> "assets/app.css")

    "app.js" ->
      Scotty.setHeader "Content-Type" "application/javascript" >>
       Scotty.file (staticPath <> "assets/app.js")

fonts :: FilePath -> ActionM ()
fonts = Scotty.file . (<>) (staticPath <> "fonts/")

images :: FilePath -> ActionM ()
images = Scotty.file . (<>) (staticPath <> "images/")

home :: ActionM ()
home =
  Scotty.html (Lucid.renderText (Layout.root Home.content))

search :: ActionM ()
search = do
    searchQuery    <- Scotty.param @Text "query"
    searchResource <- Scotty.param @Text "resource"
    searchPage     <- Scotty.param @Word "page"

    let
      searchData :: SearchData
      searchData =
        SearchData
          { sdQuery = searchQuery
          , sdResource = searchResource
          , sdPage = searchPage
          }

    case searchResource of
      "people" -> do
        peopleResults <-
          Scotty.liftAndCatchIO (searchPeople searchQuery (Page searchPage))

        renderResults searchData (Entry.fromPerson <$> peopleResults)

      "film" -> do
        filmResults <-
          Scotty.liftAndCatchIO (searchFilms searchQuery (Page searchPage))

        renderResults searchData (Entry.fromFilm <$> filmResults)

      "starship" -> do
        starshipResults <-
          Scotty.liftAndCatchIO $
            searchStarships (sdQuery searchData) (Page (sdPage searchData))

        renderResults searchData (Entry.fromStarship <$> starshipResults)

      "vehicle" -> do
        vehicleResults <-
          Scotty.liftAndCatchIO $
            searchVehicles (sdQuery searchData) (Page (sdPage searchData))

        renderResults searchData (Entry.fromVehicle <$> vehicleResults)

      "species" -> do
        speciesResults <-
          Scotty.liftAndCatchIO $
            searchSpecies (sdQuery searchData) (Page (sdPage searchData))

        renderResults searchData (Entry.fromSpecies <$> speciesResults)

      "planet" -> do
        planetResults <-
          Scotty.liftAndCatchIO $
            searchPlanets (sdQuery searchData) (Page (sdPage searchData))

        renderResults searchData (Entry.fromPlanet <$> planetResults)


      _ ->
        -- TODO: Implement own template because the default is too barebones
        Scotty.rescue
          (Scotty.raise "Invalid resource")
          (const $ Scotty.html $ Lucid.renderText $ Layout.root $ Errors.unexpectedResource searchData)

  where
    renderResults :: SearchData -> Index Entry -> ActionM ()
    renderResults searchData entries =
      let
        content :: Html ()
        content = Layout.root (Results.content searchData entries)
      in Scotty.html (Lucid.renderText (Layout.root content))

--------------------------------------------------------------------------------

staticPath :: FilePath
staticPath = "./priv/static/"

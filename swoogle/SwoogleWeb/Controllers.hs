-- | Oh look at Sekun! Trying to shoehorn MVC into a non-MVC library.
module SwoogleWeb.Controllers where

--------------------------------------------------------------------------------

import Data.Coerce                    (coerce)
import Data.List                      (foldl')
import Data.Text                      (Text)
import Data.Text.Internal.Search      qualified as Search (indices)
import Data.Text.Lazy                 qualified as Text.Lazy (toStrict)
import Lucid                          (Html)
import Lucid                          qualified (renderText)
import Network.HTTP.Types.Status      qualified as Status (status404)
import Web.Scotty                     (ActionM)
import Web.Scotty                     qualified as Scotty (file, html,
                                                           liftAndCatchIO,
                                                           param, raise,
                                                           raiseStatus, rescue,
                                                           setHeader)

--------------------------------------------------------------------------------

import Swoogle.Entry                  (Entry)
import Swoogle.Entry                  qualified as Entry
import Swoogle.SearchData

-- Components
import SwoogleWeb.Components.Search   qualified as Search (suggestionsEntry)

-- Views
import SwoogleWeb.Views.Errors        qualified as Errors
import SwoogleWeb.Views.Home          qualified as Home (content)
import SwoogleWeb.Views.Layout        qualified as Layout (noFooterRoot, root)
import SwoogleWeb.Views.People.Show   qualified as Resource.Show (content)
import SwoogleWeb.Views.SearchResults qualified as Results (content)

-- Swapi
import Swapi                          (Index (iResults), Page (Page),
                                       PersonId (PersonId), getPerson,
                                       searchFilms, searchPeople, searchPlanets,
                                       searchSpecies, searchStarships,
                                       searchVehicles)

import Swapi.Resource.Film            (Film (fTitle))
import Swapi.Resource.Person          (Person (pName), PersonName (PersonName))
import Swapi.Resource.Planet          (Planet (plName),
                                       PlanetName (MkPlanetName))
import Swapi.Resource.Species         (OriginlessSpecies (hSpName),
                                       Species (spName),
                                       SpeciesName (SpeciesName),
                                       SpeciesType (HasOrigin, NoOrigin))
import Swapi.Resource.Starship        (Starship (sName),
                                       StarshipName (StarshipName))
import Swapi.Resource.Vehicle         (Vehicle (vName),
                                       VehicleName (VehicleName))

--------------------------------------------------------------------------------
-- List of content types:
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types

assets :: Text -> ActionM ()
assets asset =
  -- TODO: Handle other cases
  case asset of
    "app.css" ->
      Scotty.setHeader "Content-Type" "text/css; charset=utf-8"
        >> Scotty.file (staticPath <> "assets/app.css")

    "app.js" ->
      Scotty.setHeader "Content-Type" "application/javascript"
        >> Scotty.file (staticPath <> "assets/app.js")

    _ -> Scotty.raiseStatus Status.status404 "That asset does not exist"

images :: FilePath -> ActionM ()
images = Scotty.file . (<>) (staticPath <> "images/")

home :: ActionM ()
home =
  Scotty.html (Lucid.renderText (Layout.root Home.content))

search :: ActionM ()
search = flip Scotty.rescue (renderException . Text.Lazy.toStrict) $ do
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
          (const
             $ Scotty.html
             $ Lucid.renderText
             $ Layout.noFooterRoot
             $ Errors.unexpectedResource searchData)

  where
    renderResults :: SearchData -> Index Entry -> ActionM ()
    renderResults searchData entries =
      let
        content :: Html ()
        content = Layout.root (Results.content searchData entries)
      in Scotty.html (Lucid.renderText content)

    renderException message =
      let search404 =
            Search.indices
              "(Response {responseStatus = Status {statusCode = 404, statusMessage = \"NOT FOUND\"}"
              message

          template =
            case search404 of
              [] -> "I have no idea what happened"
              _  -> Errors.httpNotFound
      in
        Scotty.html
          $ Lucid.renderText
          $ Layout.noFooterRoot template

suggest :: ActionM ()
suggest = do
  query    <- Scotty.param @Text "query"
  resource <- Scotty.param @Text "resource"

  case resource of
    "people" -> do
      peopleResults <- Scotty.liftAndCatchIO $ searchPeople query (Page 1)

      let
        ps :: [Person]
        ps = take 5 (iResults peopleResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions p ->
               suggestions <>
                 Search.suggestionsEntry
                   resource (coerce @PersonName @Text (pName p)))
            mempty
            ps

    "film" -> do
      filmResults <- Scotty.liftAndCatchIO $ searchFilms query (Page 1)

      let
        fs :: [Film]
        fs = take 5 (iResults filmResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions f ->
               suggestions <>
                 Search.suggestionsEntry
                   resource (fTitle f))
            mempty
            fs

    "starship" -> do
      starshipResults <- Scotty.liftAndCatchIO $ searchStarships query (Page 1)

      let
        ss :: [Starship]
        ss = take 5 (iResults starshipResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions s ->
               suggestions <>
                 Search.suggestionsEntry
                   resource (coerce @StarshipName @Text $ sName s))
            mempty
            ss

    "vehicle" -> do
      vehicleResults <- Scotty.liftAndCatchIO $ searchVehicles query (Page 1)

      let
        vs :: [Vehicle]
        vs = take 5 (iResults vehicleResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions v ->
               suggestions <>
                 Search.suggestionsEntry
                   resource (coerce @VehicleName @Text $ vName v))
            mempty
            vs

    "species" -> do
      speciesResults <- Scotty.liftAndCatchIO $ searchSpecies query (Page 1)

      let
        sps :: [SpeciesType]
        sps = take 5 (iResults speciesResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions sp ->
               suggestions <>
                 case sp of
                   HasOrigin osp ->
                       Search.suggestionsEntry
                         resource (coerce @SpeciesName @Text $ spName osp)

                   NoOrigin hsp ->
                     Search.suggestionsEntry
                       resource (coerce @SpeciesName @Text $ hSpName hsp)
            )
            mempty
            sps

    "planet" -> do
      planetResults <- Scotty.liftAndCatchIO $ searchPlanets query (Page 1)

      let
        ps :: [Planet]
        ps = take 5 (iResults planetResults)

      Scotty.html
        $ Lucid.renderText
        $ foldl'
            (\suggestions p ->
               suggestions <>
                 Search.suggestionsEntry
                   resource (coerce @PlanetName @Text $ plName p))
            mempty
            ps

    _ ->
      Scotty.raise "Unexpected category/resource"

-- TODO: Implement this
showPerson :: ActionM ()
showPerson = do
  pId <- Scotty.param @Word "id"
  person <- Scotty.liftAndCatchIO $ getPerson (PersonId pId)

  Scotty.liftAndCatchIO (print person)

  Scotty.html
    $ Lucid.renderText
    $ Layout.root
    $ Resource.Show.content person

--------------------------------------------------------------------------------

staticPath :: FilePath
staticPath = "./priv/static/"

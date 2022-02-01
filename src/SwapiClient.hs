{- |
Copyright:  © 2022 Sek Un
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Sek Un <sekun@hey.com>

![SWAPI logo](swapi-haskell.svg)

@swapi-client@ is a client library for SWAPI (@https://swapi.dev/@).

== GHCI

You can also tinker around with it in GHCI.

>>> :module + SwapiClient
>>> eitherSearchPeople "sekun" (Page 1)
Right (Index {iCount = 0, iNextPage = NoPage, iPreviousPage = NoPage, iResults = []})

/Of course, I don't exist in the Star Wars universe./

== Usage

The following examples assume you've imported `SwapiClient` like so:

@
import SwapiClient
-- ^ Imports the query functions, and common data types needed to perform a query.
@

=== List entries

@
main :: IO ()
main = do
  people <- listPeople (Page 1)

  print people
@

=== Search

@
main :: IO ()
main = do
  films <- searchFilms "empire" (Page 1)

  print films
@

=== Fetch a resource by ID

@
main :: IO ()
main = do
  lukeSkywalker <- getPerson (PersonId 1)

  print lukeSkywalker
@

You can't use the improper IDs when querying.

@
-- Bad

main :: IO ()
main = do
  lukeSkywalker <- getPerson (FilmId 1) -- GHC will complain!

  print lukeSkywalker
@

@
<interactive>:214:12: error:
    • Couldn't match expected type ‘PersonId’ with actual type ‘FilmId’
    • In the first argument of ‘getPerson’, namely ‘(FilmId 1)’
      In the expression: getPerson (FilmId 1)
      In an equation for ‘it’: it = getPerson (FilmId 1)
@

These functions evaluate to @IO (Maybe a)@ by default, @a@ being the resource.
So you'd have to deal with the @IO@ and @Maybe@ context if you want to do anything
with the value.

@
import SwapiClient.Resource.Person
-- ^ Brings in `pHomeworldId` to scope

main :: IO ()
main =
  -- 1. Fetch a person with ID of 1
  getPerson (PersonId 1) >>=
    \case
      Just person ->
        -- 2. Fetch the person's homeworld
        getPlanet (pHomeworldId person) >>= print
      Nothing -> putStrLn "Wait, there's nothing?"
@

/Mutters/ something monad transformers... /mutters/.

>>> main
IO $
  Just $
    Planet
      { plName = MkPlanetName "Tatooine"
      , plRotationPeriod = RotationPeriod 23
      , plOrbitalPeriod = OrbitalPeriod 304
      , plDiameter = Diameter 10465
      , plClimate = [Arid], plGravity = Standard 1.0, plTerrain = [Desert]
      , plSurfaceWaterPercent = Percentage 1.0
      , plPopulation = Population 200000
      , plResidents = [PersonId 1,PersonId 2,PersonId 4,PersonId 6,PersonId 7,PersonId 8,PersonId 9,PersonId 11,PersonId 43,PersonId 62]
      , plFilms = [FilmId 1,FilmId 3,FilmId 4,FilmId 5,FilmId 6]
      , plCreatedAt = 2014-12-09 13:50:49.641 UTC
      , plEditedAt = 2014-12-20 20:58:18.411 UTC, plId = PlanetId 1
      }

=== Either

There are also @IO (Either String a)@ versions of these functions, if you need
information on why a decoding failed. They're used pretty much the same way,
just in a different context. They're all prefixed with `either`.

@
main :: IO ()
main = do
  lukeSkywalker <- eitherGetPerson (PersonId 1)

  print lukeSkywalker
@


== GHC Compatibility

I've only tested this with GHC 8.10.7. Any versions before or after are not
guaranteed to work. You may try, and report your findings if you'd like.

This module only serves to re-export commonly used functions/data types, enough
to use the API calls. If you need something more specific like a resource data
constructor, or some of its fields, then you'd have to import the module directly.

-}
module SwapiClient (
  module Id,
  module Api,
  module Page,
) where

-- TODO: Streamline text pre-processing
-- TODO: Extract common behavior when parsing integers and decimal strings
-- TODO: Use `DerivingVia` for simpler `newtypes`

import SwapiClient.Api  as Api
import SwapiClient.Id   as Id
import SwapiClient.Page as Page

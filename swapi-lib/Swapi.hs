{- |
Copyright:  © 2022 Sek Un
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Sek Un <sekun@hey.com>

![SWAPI logo](swapi-haskell.svg)

@swapi-client@ is a client library for SWAPI (@https://swapi.dev/@).

== GHCI

You can also tinker around with it in GHCI.

>>> :module + Swapi
>>> eitherSearchPeople "sekun" (Page 1)
Right (Index {iCount = 0, iNextPage = NoPage, iPreviousPage = NoPage, iResults = []})

/Of course, I don't exist in the Star Wars universe./

== Usage

The following examples assume you've imported `Swapi` like so:

@
import Swapi
-- ^ Imports the query functions, and common data types needed to perform a query.
@

=== List entries

@
main :: IO ()
main = do
  -- @people@ has a type @Index People@
  people <- listPeople (Page 1)
            -- ^ Evaluates to @IO (Index People)@

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

@
import Swapi
import Swapi.Resource.Person
-- ^ Brings in `pHomeworldId` to scope

-- | Prints the homeworld data of a character
main :: IO ()
main =
  getPerson (PersonId 2)
  -- ^ Fetches info about a character with an ID 2
    >>= \person -> getPlanet (pHomeworldId person)
    -- ^ Gets information about a character's homeworld
    >>= print
@

You could also do it with do-notation

@
main = do
  person <- getPerson (PersonId 2)
  planet <- getPlanet (pHomeworldId person)

  print planet
@

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
module Swapi (
  module Id,
  module Api,
  module Page,
) where

-- TODO: Streamline text pre-processing
-- TODO: Extract common behavior when parsing integers and decimal strings
-- TODO: Use `DerivingVia` for simpler `newtypes`
-- TODO: Actually use doctests
-- TODO: Fix doctests
-- TODO: Move internal modules to `Internal.*`

import Swapi.Api           as Api
import Swapi.Id            as Id
import Swapi.Internal.Page as Page

module Main where

import SwapiClient
import SwapiClient.Resource.Person

main :: IO ()
main =
  getPerson (PersonId 2)
    >>= \person -> getPlanet (pHomeworldId person)
    >>= print

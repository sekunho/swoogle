module Main where

import SwapiClient.Api qualified as Api (getFilm)
import SwapiClient.Id (FilmId (FilmId))

main :: IO ()
main = do
  film <- Api.getFilm (FilmId 1)

  print film

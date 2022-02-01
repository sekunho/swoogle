module Main where

import SwapiClient (FilmId (FilmId), getFilm)

main :: IO ()
main = do
  film <- getFilm (FilmId 1)

  print film

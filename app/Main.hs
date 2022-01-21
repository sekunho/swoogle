module Main where

import SwapiClient (getFilm, FilmId (FilmId))

main :: IO ()
main = do
  film <- getFilm (FilmId 1)

  print film

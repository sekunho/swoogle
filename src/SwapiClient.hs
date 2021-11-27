module SwapiClient
  ( Person (..)
  , PersonName (PersonName)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , HairColor (HairColor)
  , SkinColor (SkinColor)
  , EyeColor (EyeColor)
  , BirthYear (ABY, BBY, UnknownBirthYear)
  , HomeworldId (HomeworldId)
  , FilmId (FilmId)
  , Gender (Male, Female)
  , lukeSkywalker -- TODO: Remove because I'm only using this for convenience
  ) where

import SwapiClient.Person
import SwapiClient.Homeworld
import SwapiClient.Film

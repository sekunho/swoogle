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
  , Resource (People, Film, Planet)
  , lukeSkywalker -- TODO: Remove because I'm only using this for convenience
  ) where

import SwapiClient.Person
import SwapiClient.Homeworld ()
import SwapiClient.Film ()
import SwapiClient.Id (FilmId (FilmId), HomeworldId (HomeworldId))
import SwapiClient.Url (Resource (People, Film, Planet))

module SwapiClient
  ( Person (..)
  , PersonName (PersonName)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , HairColor (HairColor)
  , SkinColor (SkinColor)
  , EyeColor (EyeColor)
  , BirthYear (ABY, BBY, UnknownBirthYear)
  , HomeworldId
  , FilmId
  , Gender (Male, Female)
  , Resource (People, Film, Planet)
  , lukeSkywalker
  ) where

import SwapiClient.Person
import SwapiClient.Homeworld ()
import SwapiClient.Film ()
import SwapiClient.Id (FilmId, HomeworldId)
import SwapiClient.Url (Resource (People, Film, Planet))

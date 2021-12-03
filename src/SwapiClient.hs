module SwapiClient
  ( Person (..)
  , PersonName (PersonName)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
  , BirthYear (ABY, BBY, UnknownBirthYear)

  -- IDs and their smart constructors
  , FilmId
  , HomeworldId
  , SpeciesId
  , VehicleId
  , StarshipId
  , PersonId
  , mkFilmId
  , unFilmId
  , mkHomeworldId
  , unHomeworldId
  , mkSpeciesId
  , unSpeciesId
  , mkVehicleId
  , unVehicleId
  , mkStarshipId
  , unStarshipId
  , mkPersonId
  , unPersonId

  , Gender (Male, Female, Hermaphrodite, NoGender)
  , Resource (People, Film, Planet)
  , HairColors (HairColors)
  , HairColor
    ( AuburnHair
    , BlackHair
    , BlondHair
    , BrownHair
    , GreyHair
    , NoHairColor
    , WhiteHair
    )
  , SkinColor
    ( BlueSkin
    , BrownSkin
    , BrownMottleSkin
    , DarkSkin
    , FairSkin
    , GoldSkin
    , GreenSkin
    , GreenTanSkin
    , GreySkin
    , LightSkin
    , MetalSkin
    , MottledGreenSkin
    , OrangeSkin
    , PaleSkin
    , RedSkin
    , SilverSkin
    , TanSkin
    , UnknownSkinColor
    , WhiteSkin
    , YellowSkin
    )
  , EyeColor
    ( BlackEye
    , BlueEye
    , BlueGreyEye
    , GoldEye
    , HazelEye
    , OrangeEye
    , PinkEye
    , RedEye
    , UnknownEyeColor
    , YellowEye
    )
  ) where

import SwapiClient.Person
import SwapiClient.Homeworld ()
import SwapiClient.Film ()
import SwapiClient.Id
  ( FilmId
  , HomeworldId
  , SpeciesId
  , VehicleId
  , StarshipId
  , PersonId
  , mkFilmId
  , unFilmId
  , mkHomeworldId
  , unHomeworldId
  , mkSpeciesId
  , unSpeciesId
  , mkVehicleId
  , unVehicleId
  , mkStarshipId
  , unStarshipId
  , mkPersonId
  , unPersonId
  )
import SwapiClient.Url (Resource (People, Film, Planet))
import SwapiClient.Color
  ( HairColors (HairColors)
  , HairColor
    ( AuburnHair
    , BlackHair
    , BlondHair
    , BrownHair
    , GreyHair
    , NoHairColor
    , WhiteHair
    )
  , SkinColor
    ( BlueSkin
    , BrownSkin
    , BrownMottleSkin
    , DarkSkin
    , FairSkin
    , GoldSkin
    , GreenSkin
    , GreenTanSkin
    , GreySkin
    , LightSkin
    , MetalSkin
    , MottledGreenSkin
    , OrangeSkin
    , PaleSkin
    , RedSkin
    , SilverSkin
    , TanSkin
    , UnknownSkinColor
    , WhiteSkin
    , YellowSkin
    )
  , EyeColor
    ( BlackEye
    , BlueEye
    , BlueGreyEye
    , GoldEye
    , HazelEye
    , OrangeEye
    , PinkEye
    , RedEye
    , UnknownEyeColor
    , YellowEye
    )
  )

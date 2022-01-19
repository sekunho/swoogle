module SwapiClient
  ( Person (..)
  , Root (..)
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
  , unFilmId
  , unHomeworldId
  , unSpeciesId
  , unVehicleId
  , unStarshipId
  , unPersonId
  , Gender (Male, Female, Hermaphrodite, NoGender)
  , Resource (People, Film, Planet)
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
  , unFilmId
  , unHomeworldId
  , unSpeciesId
  , unVehicleId
  , unStarshipId
  , unPersonId
  )
import SwapiClient.Url (Resource (People, Film, Planet))
import SwapiClient.Color
  ( HairColor
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
import SwapiClient.Root (Root (..))

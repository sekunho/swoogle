module SwapiClient
  ( Person (..)
  , PersonName (PersonName)
  , Height (Height, UnknownHeight)
  , Mass (Mass, UnknownMass)
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

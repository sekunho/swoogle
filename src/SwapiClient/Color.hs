module SwapiClient.Color
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
  , SkinColors (SkinColors)
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

import Data.Aeson qualified as Aeson (withText)
import Data.Aeson.Types
    ( FromJSON(parseJSON)
    , Value(String)
    , ToJSON(toJSON)
    , Parser
    )
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text (splitOn, intercalate)
import TextShow ( Builder, TextShow(showb), showt)

--------------------------------------------------------------------------------
-- Data types

data HairColor
  = AuburnHair
  | BlackHair
  | BlondHair
  | BrownHair
  | GreyHair
  | NoHairColor
  | WhiteHair
  deriving Show

newtype HairColors = HairColors [HairColor]
  deriving Show

data SkinColor
  = BlueSkin
  | BrownSkin
  | BrownMottleSkin
  | DarkSkin
  | FairSkin
  | GoldSkin
  | GreenSkin
  | GreenTanSkin
  | GreySkin
  | LightSkin
  | MetalSkin
  | MottledGreenSkin
  | OrangeSkin
  | PaleSkin
  | RedSkin
  | SilverSkin
  | TanSkin
  | UnknownSkinColor
  | WhiteSkin
  | YellowSkin
  deriving Show

newtype SkinColors = SkinColors [SkinColor]
  deriving Show

data EyeColor
  = BlackEye
  | BlueEye
  | BlueGreyEye
  | GoldEye
  | HazelEye
  | OrangeEye
  | PinkEye -- lmao
  | RedEye
  | UnknownEyeColor
  | YellowEye
  deriving Show

-------------------------------------------------------------------------------
-- Instances

-- TextShow instances

instance TextShow (HairColor :: Type) where
  showb :: HairColor -> Builder
  showb = \case
    AuburnHair -> "auburn"
    BlackHair -> "black"
    BlondHair -> "blond"
    BrownHair -> "brown"
    GreyHair -> "grey"
    NoHairColor -> "none"
    WhiteHair -> "white"

instance TextShow (SkinColor :: Type) where
  showb :: SkinColor -> Builder
  showb = \case
    BlueSkin -> "blue"
    BrownSkin -> "brown"
    BrownMottleSkin -> "brown mottle"
    DarkSkin -> "dark"
    FairSkin -> "fair"
    GoldSkin -> "gold"
    GreenSkin -> "green"
    GreenTanSkin -> "green-tan"
    GreySkin -> "grey"
    LightSkin -> "light"
    MetalSkin -> "metal"
    MottledGreenSkin -> "mottled green"
    OrangeSkin -> "orange"
    PaleSkin -> "pale"
    RedSkin -> "red"
    SilverSkin -> "silver"
    TanSkin -> "tan"
    UnknownSkinColor -> "unknown"
    WhiteSkin -> "white"
    YellowSkin -> "yellow"

instance TextShow (EyeColor :: Type) where
  showb :: EyeColor -> Builder
  showb = \case
    BlackEye -> "black"
    BlueEye -> "blue"
    BlueGreyEye -> "blue-grey"
    GoldEye -> "gold"
    HazelEye -> "hazel"
    OrangeEye -> "orange"
    PinkEye -> "pink"
    RedEye -> "red"
    UnknownEyeColor -> "unknown"
    YellowEye -> "yellow"

-- Aeson instances

instance FromJSON (HairColors :: Type) where
  parseJSON :: Value -> Parser HairColors
  parseJSON =
   Aeson.withText "HairColor"
      $ \hairText ->
          let hairColors :: Either String [HairColor]
              hairColors = mapM textToHairColor . Text.splitOn ", " $ hairText
          in
            case hairColors of
              Right hc -> pure . HairColors $ hc
              Left e -> fail e

instance ToJSON (HairColors :: Type) where
  toJSON :: HairColors -> Value
  toJSON = String . commaConcat . unHairColors

instance FromJSON (SkinColors :: Type) where
  parseJSON :: Value -> Parser SkinColors
  parseJSON =
    Aeson.withText "SkinColors" $
      \skinColorText ->
        let textToColors :: Text -> Either String [SkinColor]
            textToColors = mapM textToSkinColor . Text.splitOn ", "
        in
          case textToColors skinColorText of
            Right skinColors -> pure . SkinColors $ skinColors
            Left e -> fail e

instance ToJSON (SkinColors :: Type) where
  toJSON :: SkinColors -> Value
  toJSON = String . commaConcat . unSkinColors

instance FromJSON (EyeColor :: Type) where
  parseJSON :: Value -> Parser EyeColor
  parseJSON =
   Aeson.withText "EyeColor" $
     \case
       "black" -> pure BlackEye
       "blue" -> pure BlueEye
       "blue-grey" -> pure BlueGreyEye
       "gold" -> pure GoldEye
       "hazel" -> pure HazelEye
       "orange" -> pure OrangeEye
       "pink" -> pure PinkEye
       "red" -> pure RedEye
       "unknown" -> pure UnknownEyeColor
       "yellow" -> pure YellowEye
       _ -> fail "ERROR: Invalid eye color value/format"

instance ToJSON (EyeColor :: Type) where
  toJSON :: EyeColor -> Value
  toJSON = String . showt

--------------------------------------------------------------------------------
-- Functions

textToHairColor :: Text -> Either String HairColor
textToHairColor hct = case hct of
  "auburn"     -> Right AuburnHair
  "black"      -> Right BlackHair
  "blond"      -> Right BlondHair
  "brown"      -> Right BrownHair
  "grey"       -> Right GreyHair
  "white"      -> Right WhiteHair
  "n/a"        -> Right NoHairColor
  "none"       -> Right NoHairColor
  _            -> Left "ERROR: Invalid hair color value/format"

textToSkinColor :: Text -> Either String SkinColor
textToSkinColor sct = case sct of
  "blue" -> Right BlueSkin
  "brown" -> Right BrownSkin
  "brown mottle" -> Right BrownMottleSkin
  "dark" -> Right DarkSkin
  "fair" -> Right FairSkin
  "gold" -> Right GoldSkin
  "green" -> Right GreenSkin
  "green-tan" -> Right GreenTanSkin
  "grey" -> Right GreySkin
  "light" -> Right LightSkin
  "metal" -> Right MetalSkin
  "mottled green" -> Right MottledGreenSkin
  "orange" -> Right OrangeSkin
  "pale" -> Right PaleSkin
  "red" -> Right RedSkin
  "silver" -> Right SilverSkin
  "tan" -> Right TanSkin
  "white" -> Right WhiteSkin
  "yellow" -> Right YellowSkin
  "unknown" -> Right UnknownSkinColor
  _ -> Left "ERROR: Invalid skin color/format"

--------------------------------------------------------------------------------
-- Newtype functions

unHairColors :: HairColors -> [HairColor]
unHairColors (HairColors hcs) = hcs

unSkinColors :: SkinColors -> [SkinColor]
unSkinColors (SkinColors scs) = scs

--------------------------------------------------------------------------------
-- Utils

commaConcat :: TextShow a => [a] -> Text
commaConcat = Text.intercalate ", " .  map showt

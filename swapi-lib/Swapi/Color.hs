module Swapi.Color
  ( HairColor (..)
  , SkinColor (..)
  , EyeColor (..)
  ) where

--------------------------------------------------------------------------------

import Data.Aeson       qualified as Aeson (withText)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, ToJSON (toJSON),
                         Value (String))
import Data.Kind        (Type)
import Data.Text        (Text)
import Data.Text        qualified as Text (intercalate, splitOn, unpack)
import TextShow         (Builder, TextShow (showb), showt)

--------------------------------------------------------------------------------
-- Data types

data HairColor
  = AuburnHair
  | BlackHair
  | BlondHair
  | BrownHair
  | GreyHair
  | WhiteHair
  | RedHair
  | BlueHair
  | NoHairColor
  | UnknownHairColor
  deriving (Eq, Show)

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
  | MagentaSkin
  | MetalSkin
  | MottledGreenSkin
  | OrangeSkin
  | PaleSkin
  | RedSkin
  | SilverSkin
  | TanSkin
  | WhiteSkin
  | YellowSkin
  | CaucasianSkin
  | BlackSkin
  | AsianSkin
  | HispanicSkin
  | PurpleSkin
  | PinkSkin
  | PalePinkSkin
  | PeachSkin
  | SkinColorNotApplicable
  | UnknownSkinColor
  deriving (Eq, Show)

data EyeColor
  = AmberEye
  | BlackEye
  | BlueEye
  | BlueGreyEye
  | BrownEye
  | GoldEye
  | GreenEye
  | GreyEye
  | HazelEye
  | OrangeEye
  | PinkEye -- lmao
  | RedEye
  | YellowEye
  | WhiteEye
  | IndigoEye
  | SilverEye
  | EyeColorNotApplicable
  | UnknownEyeColor
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Instances

-- TextShow instances

instance TextShow (HairColor :: Type) where
  showb :: HairColor -> Builder
  showb = \case
    AuburnHair       -> "auburn"
    BlackHair        -> "black"
    BlondHair        -> "blond"
    BrownHair        -> "brown"
    GreyHair         -> "grey"
    WhiteHair        -> "white"
    RedHair          -> "red"
    BlueHair         -> "blue"
    UnknownHairColor -> "unknown"
    NoHairColor      -> "none"

instance TextShow (SkinColor :: Type) where
  showb :: SkinColor -> Builder
  showb = \case
    BlueSkin               -> "blue"
    BrownSkin              -> "brown"
    BrownMottleSkin        -> "brown mottle"
    DarkSkin               -> "dark"
    FairSkin               -> "fair"
    GoldSkin               -> "gold"
    GreenSkin              -> "green"
    GreenTanSkin           -> "green-tan"
    GreySkin               -> "grey"
    LightSkin              -> "light"
    MagentaSkin            -> "magenta"
    MetalSkin              -> "metal"
    MottledGreenSkin       -> "mottled green"
    OrangeSkin             -> "orange"
    PaleSkin               -> "pale"
    RedSkin                -> "red"
    SilverSkin             -> "silver"
    TanSkin                -> "tan"
    WhiteSkin              -> "white"
    YellowSkin             -> "yellow"
    CaucasianSkin          -> "caucasian"
    BlackSkin              -> "black"
    AsianSkin              -> "asian"
    HispanicSkin           -> "hispanic"
    PurpleSkin             -> "purple"
    PinkSkin               -> "pink"
    PalePinkSkin           -> "pale pink"
    PeachSkin              -> "peach"
    SkinColorNotApplicable -> "n/a"
    UnknownSkinColor       -> "unknown"

instance TextShow (EyeColor :: Type) where
  showb :: EyeColor -> Builder
  showb = \case
    BlackEye              -> "black"
    BlueEye               -> "blue"
    BlueGreyEye           -> "blue-grey"
    BrownEye              -> "brown"
    GoldEye               -> "gold"
    GreenEye              -> "green"
    HazelEye              -> "hazel"
    OrangeEye             -> "orange"
    PinkEye               -> "pink"
    RedEye                -> "red"
    YellowEye             -> "yellow"
    WhiteEye              -> "white"
    AmberEye              -> "amber"
    GreyEye               -> "grey"
    IndigoEye             -> "indigo"
    SilverEye             -> "silver"
    EyeColorNotApplicable -> "n/a"
    UnknownEyeColor       -> "unknown"

-- Aeson instances

instance {-# OVERLAPS #-} FromJSON ([HairColor] :: Type) where
  parseJSON :: Value -> Parser [HairColor]
  parseJSON =
   Aeson.withText "HairColor"
      $ \hairText ->
          let hairColors :: Either String [HairColor]
              hairColors = mapM textToHairColor . Text.splitOn ", " $ hairText
          in
            case hairColors of
              Right hc -> pure hc
              Left e   -> fail e

instance {-# OVERLAPS #-} ToJSON ([HairColor] :: Type) where
  toJSON :: [HairColor] -> Value
  toJSON = String . commaConcat

instance {-# OVERLAPS #-} FromJSON ([SkinColor] :: Type) where
  parseJSON :: Value -> Parser [SkinColor]
  parseJSON =
    Aeson.withText "SkinColors" $
      \skinColorText ->
        let textToColors :: Text -> Either String [SkinColor]
            textToColors = mapM textToSkinColor . Text.splitOn ", "
        in
          case textToColors skinColorText of
            Right skinColors -> pure skinColors
            Left e           -> fail e

instance {-# OVERLAPS #-} ToJSON ([SkinColor] :: Type) where
  toJSON :: [SkinColor] -> Value
  toJSON = String . commaConcat

instance FromJSON (EyeColor :: Type) where
  parseJSON :: Value -> Parser EyeColor
  parseJSON =
   Aeson.withText "EyeColor" $
     \val ->
       case textToEyeColor val of
         Right eyeColor -> pure eyeColor
         Left e         -> fail e

instance ToJSON (EyeColor :: Type) where
  toJSON :: EyeColor -> Value
  toJSON = String . showt

instance {-# OVERLAPS #-} FromJSON ([EyeColor] :: Type) where
  parseJSON :: Value -> Parser [EyeColor]
  parseJSON =
    Aeson.withText "[EyeColor]" $
      \eyeColorsText ->
        let
          textToColors :: Text -> Either String [EyeColor]
          textToColors = mapM textToEyeColor . Text.splitOn ", "
        in
          case textToColors eyeColorsText of
            Right eyeColors -> pure eyeColors
            Left e          -> fail e

--------------------------------------------------------------------------------
-- Functions

textToHairColor :: Text -> Either String HairColor
textToHairColor hct = case hct of
  "auburn"  -> Right AuburnHair
  "black"   -> Right BlackHair
  "blond"   -> Right BlondHair
  "blonde"  -> Right BlondHair
  "brown"   -> Right BrownHair
  "grey"    -> Right GreyHair
  "white"   -> Right WhiteHair
  "blue"    -> Right BlueHair
  "red"     -> Right RedHair
  "n/a"     -> Right NoHairColor
  "none"    -> Right NoHairColor
  "unknown" -> Right UnknownHairColor
  h         -> Left ("ERROR: Invalid hair color value/format: " <> Text.unpack h)

textToSkinColor :: Text -> Either String SkinColor
textToSkinColor sct = case sct of
  "blue"          -> Right BlueSkin
  "brown"         -> Right BrownSkin
  "brown mottle"  -> Right BrownMottleSkin
  "dark"          -> Right DarkSkin
  "fair"          -> Right FairSkin
  "gold"          -> Right GoldSkin
  "green"         -> Right GreenSkin
  "green-tan"     -> Right GreenTanSkin
  "grey"          -> Right GreySkin
  "gray"          -> Right GreySkin
  "light"         -> Right LightSkin
  "magenta"       -> Right MagentaSkin
  "metal"         -> Right MetalSkin
  "mottled green" -> Right MottledGreenSkin
  "orange"        -> Right OrangeSkin
  "pale"          -> Right PaleSkin
  "red"           -> Right RedSkin
  "silver"        -> Right SilverSkin
  "tan"           -> Right TanSkin
  "white"         -> Right WhiteSkin
  "yellow"        -> Right YellowSkin
  "caucasian"     -> Right CaucasianSkin
  "black"         -> Right BlackSkin
  "asian"         -> Right AsianSkin
  "hispanic"      -> Right HispanicSkin
  "purple"        -> Right PurpleSkin
  "pink"          -> Right PinkSkin
  "pale pink"     -> Right PalePinkSkin
  "peach"         -> Right PeachSkin
  "unknown"       -> Right UnknownSkinColor
  "n/a"           -> Right SkinColorNotApplicable
  s               -> Left ("ERROR: Invalid skin color/format " <> Text.unpack s)

textToEyeColor :: Text -> Either String EyeColor
textToEyeColor ect = case ect of
  "black" -> pure BlackEye
  "blue" -> pure BlueEye
  "blue-gray" -> pure BlueGreyEye
  "blue-grey" -> pure BlueGreyEye
  "brown" -> pure BrownEye
  "gold" -> pure GoldEye
  "golden" -> pure GoldEye
  "green" -> pure GreenEye
  "hazel" -> pure HazelEye
  "orange" -> pure OrangeEye
  "pink" -> pure PinkEye
  "red" -> pure RedEye
  "yellow" -> pure YellowEye
  "amber" -> pure AmberEye
  "grey" -> pure GreyEye
  "indigo" -> pure IndigoEye
  "silver" -> pure SilverEye
  "unknown" -> pure UnknownEyeColor
  "n/a" -> pure EyeColorNotApplicable
  e -> Left ("ERROR: Invalid eye color value/format " <> Text.unpack e)


--------------------------------------------------------------------------------
-- Utils

-- | Concatenates a list of `TextShow`ables into a comma delimited string.
--
-- λ> commaConcat [BrownHair, BrownHair, BrownHair]
--
-- "brown, brown, brown"
commaConcat :: TextShow a => [a] -> Text
commaConcat = Text.intercalate ", " .  map showt

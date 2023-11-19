{-# LANGUAGE OverloadedStrings #-}
module Wavefront.Private.Material where

import qualified Linear
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Functor ((<&>))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Text.Parsec
    ( alphaNum,
      char,
      digit,
      oneOf,
      satisfy,
      space,
      spaces,
      string,
      endBy,
      eof,
      many1,
      manyTill,
      optionMaybe,
      optional,
      sepBy,
      (<?>),
      (<|>),
      lookAhead,
      many,
      parse,
      skipMany,
      try,
      ParseError,
      Parsec )

-- https://www.loc.gov/preservation/digital/formats/fdd/fdd000508.shtml#notes
data IlluminationMode =
    ColorOffAndAmbientOff
  | ColorAndAmbient
  | Highlight
  | ReflectionAndRayTrace
  | TransparencyGlassReflectionRayTrace
  | ReflectionFresnelAndRayTrace
  | TransparencyRefractionFresnelOffRayTrace
  | TransparencyRefractionFresnelRayTrace
  | ReflectionAndRayTraceOff
  | TransparencyGlassReflectionRayTraceOff
  | CastShadowsOntoInvisibleSurfaces
  deriving (Enum, Show, Eq, Ord)


data FilterColorMode =
    RGB (Linear.V3 Float)
  | CIEXYZ (Linear.V3 Float)
  | Spectral (FilePath, Float)
  deriving (Show, Eq)

data Material = Material {
      materialName                    :: !String                                        -- newmtl
    , materialAmbientColor            :: !(Linear.V3 Float)                             -- Ka
    , materialDiffuseColor            :: !(Linear.V3 Float)                             -- Kd
    , materialSpecularColor           :: !(Linear.V3 Float)                             -- Ks
    , materialEmmissive               :: !(Linear.V3 Float)                             -- Ke
    , materialSpecularHighlights      :: !(Maybe Float)                                 -- Ns
    , materialOpticalDensity          :: !(Maybe Float)                                 -- Ni
    , materialDissolve                :: !(Maybe Float)                                 -- d or Tr (Tr = 1 - d)
    , materialIlluminationMode        :: !IlluminationMode                              -- illum

    , materialTransmissionFilterColor :: !(Maybe FilterColorMode)                       -- Tf
    , materialMapAmbientColor         :: !(Maybe (FilePath       , TextureMapOptions))  -- map_Ka
    , materialMapDiffuseColor         :: !(Maybe (FilePath       , TextureMapOptions))  -- map_Kd
    , materialMapSpecularColor        :: !(Maybe (FilePath       , TextureMapOptions))  -- map_Ks
    , materialMapSpecularHighlights   :: !(Maybe (FilePath       , TextureMapOptions))  -- map_Ns
    , materialMapOpticalDensity       :: !(Maybe (FilePath       , TextureMapOptions))  -- map_Ni
    , materialMapDissolve             :: !(Maybe (FilePath       , TextureMapOptions))  -- map_d
    , materialMapBump                 :: !(Maybe (FilePath       , TextureMapOptions))  -- map_bump or bump
    , materialMapDisplacement         :: !(Maybe (FilePath       , TextureMapOptions))  -- disp
    , materialMapDecal                :: !(Maybe (Float          , TextureMapOptions))  -- decal 
} deriving (Show, Eq)

initMaterial:: Material
initMaterial = Material {
      materialName                    = ""
    , materialAmbientColor            = Linear.V3 0 0 0
    , materialDiffuseColor            = Linear.V3 0 0 0
    , materialSpecularColor           = Linear.V3 0 0 0
    , materialEmmissive               = Linear.V3 0 0 0
    , materialSpecularHighlights      = Nothing
    , materialOpticalDensity          = Nothing
    , materialDissolve                = Nothing
    , materialIlluminationMode        = ColorAndAmbient

    , materialTransmissionFilterColor = Nothing
    , materialMapAmbientColor         = Nothing
    , materialMapDiffuseColor         = Nothing
    , materialMapSpecularColor        = Nothing
    , materialMapSpecularHighlights   = Nothing
    , materialMapOpticalDensity       = Nothing
    , materialMapDissolve             = Nothing
    , materialMapBump                 = Nothing
    , materialMapDisplacement         = Nothing
    , materialMapDecal                = Nothing
  }

data ScalarOrBump = R | G | B | M | L | Z deriving (Show, Enum, Eq)

data TextureMapOptions = TextureMapOptions {
      textureMapOptionsHorizontalBlending   :: !Bool               -- blendu
    , textureMapOptionsVerticalBlending     :: !Bool               -- blendv
    , textureMapOptionsBoost                :: !Float              -- boost
    , textureMapOptionsModify               :: !(Linear.V2 Float)  -- mm
    , textureMapOptionsOriginOffset         :: !(Linear.V3 Float)  -- o
    , textureMapOptionsScale                :: !(Linear.V3 Float)  -- s
    , textureMapOptionsTurbulence           :: !(Linear.V3 Float)  -- t
    , textureMapOptionsResolution           :: !Float              -- texres
    , textureMapOptionsClamp                :: !Bool               -- clamp
    , textureMapOptionsBumpMultiplier       :: !Float              -- bm
    , textureMapOptionsChannel              :: !ScalarOrBump       -- imfchan r | g | b | m | l | z
} deriving (Show, Eq)


defaultTextureBumpMapOptions:: TextureMapOptions
defaultTextureBumpMapOptions = TextureMapOptions {
      textureMapOptionsHorizontalBlending  = True
    , textureMapOptionsVerticalBlending    = True
    , textureMapOptionsBoost               = 0 -- Not sure
    , textureMapOptionsModify              = Linear.V2 0 1
    , textureMapOptionsOriginOffset        = Linear.V3 0 0 0
    , textureMapOptionsScale               = Linear.V3 1 1 1
    , textureMapOptionsTurbulence          = Linear.V3 0 0 0
    , textureMapOptionsResolution          = 0 -- Not sure
    , textureMapOptionsClamp               = False
    , textureMapOptionsBumpMultiplier      = 1
    , textureMapOptionsChannel             = L
}

defaultTextureDecalMapOptions:: TextureMapOptions
defaultTextureDecalMapOptions = defaultTextureBumpMapOptions { textureMapOptionsChannel = M}

type MaterialParser a = Parsec Text.Text () a

data OptionsToken =
    Blendu     !Bool
  | Blendv     !Bool
  | Boost      !Float
  | Mm         !(Linear.V2 Float)
  | O          !(Linear.V3 Float)
  | S          !(Linear.V3 Float)
  | T          !(Linear.V3 Float)
  | Texres     !Float
  | Clamp      !Bool
  | Bm         !Float
  | Imfchan    !ScalarOrBump
  deriving Show

data Token =
    Newmtl  !String
  | Ka      !(Linear.V3 Float)
  | Kd      !(Linear.V3 Float)
  | Ks      !(Linear.V3 Float)
  | Ke      !(Linear.V3 Float)
  | Ns      !(Maybe Float)
  | Ni      !(Maybe Float)
  | D       !(Maybe Float)
  | Illum   !IlluminationMode
  | Tf      !(Maybe FilterColorMode)
  | MapKa   !(Maybe (FilePath, TextureMapOptions))
  | MapKd   !(Maybe (FilePath, TextureMapOptions))
  | MapKs   !(Maybe (FilePath, TextureMapOptions))
  | MapNs   !(Maybe (FilePath, TextureMapOptions))
  | MapNi   !(Maybe (FilePath, TextureMapOptions))
  | MapD    !(Maybe (FilePath, TextureMapOptions))
  | MapBump !(Maybe (FilePath, TextureMapOptions))
  | Disp    !(Maybe (FilePath, TextureMapOptions))
  | Decal   !(Maybe (Float   , TextureMapOptions))
  | Comment
  deriving Show


parseMtlFromFile :: FilePath -> IO (Either ParseError [Material])
parseMtlFromFile path = Text.IO.readFile path <&> parseMtl (Just path)

parseMtl:: Maybe String -> Text.Text -> Either ParseError [Material]
parseMtl (Just name) contents = parse topLevel name contents
parseMtl Nothing     contents = parse topLevel "" contents

--------------

float:: MaterialParser Float
float = do
  full <- many1 digit
  _ <- char '.'
  part <- many1 digit
  return $ read (full ++ '.':part)


parseMaterialName:: MaterialParser Token
parseMaterialName = do
  _ <- string "newmtl" <* space
  many alphaNum <&> Newmtl

parseVector3:: String -> MaterialParser (Linear.V3 Float)
parseVector3 pattern = do
  _ <- string pattern <* space
  x <- float <* space
  y <- float <* space
  z <- float
  return $ Linear.V3 x y z

parseVector2:: String -> MaterialParser (Linear.V2 Float)
parseVector2 pattern = do
  _ <- string pattern <* space
  x <- float <* space
  y <- float
  return $ Linear.V2 x y

parseSingleFloat:: String -> MaterialParser Float
parseSingleFloat pattern = do
  _ <- string pattern <* space
  x <- float
  return x

parseMapInput:: String -> MaterialParser (FilePath, TextureMapOptions)
parseMapInput pattern = do
  void $ string pattern <* space
  optionTokens <- optionsFactor `endBy` space
  let finalOptions = foldr writeOptionTokenToOptions defaultTextureBumpMapOptions optionTokens
  fileName <- many (alphaNum <|> oneOf ".-_")
  return (fileName, finalOptions)

parseIllumination:: MaterialParser IlluminationMode
parseIllumination = do
  _ <- string "illum" <* space
  enum <- many1 digit
  return $ toEnum (read enum - 1)

parseBool:: String -> MaterialParser Bool
parseBool pattern = do
  _ <- string pattern <* space
  b <- string "true" <|> string "false"
  case b of "true" -> return True
            "false" -> return False
            _       -> fail $ "Couldn't parse non boolean: " ++ show b

parseKa:: MaterialParser Token
parseKa = parseVector3 "Ka" <&> Ka

parseKd:: MaterialParser Token
parseKd = parseVector3 "Kd" <&> Kd

parseKs:: MaterialParser Token
parseKs = parseVector3 "Ks" <&> Ks

parseKe:: MaterialParser Token
parseKe = parseVector3 "Ke" <&> Ke

parseNs:: MaterialParser Token
parseNs = parseSingleFloat "Ns" <&> Ns . Just

parseNi:: MaterialParser Token
parseNi = parseSingleFloat "Ni" <&> Ni . Just

parseD:: MaterialParser Token
parseD = (parseSingleFloat "d" <&> D . Just) <|> (parseSingleFloat "Tr" <&> D . Just)

parseIllum:: MaterialParser Token
parseIllum = parseIllumination <&> Illum

parseTf:: MaterialParser Token
parseTf = try rgb <|> try ciexyz <|> try spectral
  where
    rgb = parseVector3 "Tf" >>= \x -> return $ Tf $ Just $ RGB x
    ciexyz = parseVector3 "Tf xyz" >>= \x -> return $ Tf $ Just $ CIEXYZ x
    spectral = do
      void $ string "Tf spectral" <* space
      fileName <- many (alphaNum <|> oneOf ".-_")
      m_factor <- optionMaybe $ space *> float
      return $ Tf $ Just $ Spectral (fileName, fromMaybe 1 m_factor)

parseMapKa:: MaterialParser Token
parseMapKa = parseMapInput "map_Ka" <&> MapKa . Just
parseMapKd:: MaterialParser Token
parseMapKd = parseMapInput "map_Kd" <&> MapKd . Just
parseMapKs:: MaterialParser Token
parseMapKs = parseMapInput "map_Ks" <&> MapKs . Just
parseMapNs:: MaterialParser Token
parseMapNs = parseMapInput "map_Ns" <&> MapNs . Just
parseMapNi:: MaterialParser Token
parseMapNi = parseMapInput "map_Ni" <&> MapNi . Just
parseMapD:: MaterialParser Token
parseMapD = parseMapInput "map_D" <&> MapD . Just
parseMapBump:: MaterialParser Token
parseMapBump = (parseMapInput "map_bump" <|> parseMapInput "bump") <&> MapBump . Just
parseDisp:: MaterialParser Token
parseDisp = parseMapInput "disp" <&> Disp . Just
parseDecal:: MaterialParser Token
parseDecal = do
  void $ string "decal" <* space
  optionTokens <- optionsFactor `endBy` space
  let finalOptions = foldr writeOptionTokenToOptions defaultTextureBumpMapOptions optionTokens
  floatVal <- float
  return $ Decal $ Just (floatVal, finalOptions)

comment:: MaterialParser Token
comment = do
  void $ string "#"
  skipMany (satisfy (/= '\n'))
  return Comment

expr:: MaterialParser Token
expr = do
      try parseKa
  <|> try parseKd
  <|> try parseKs
  <|> try parseKe
  <|> try parseNs
  <|> try parseNi
  <|> try parseD
  <|> try parseTf
  <|> try parseIllum
  <|> try parseMapKa
  <|> try parseMapKd
  <|> try parseMapKs
  <|> try parseMapNi
  <|> try parseMapNs
  <|> try parseMapD
  <|> try parseMapBump
  <|> try parseDisp
  <|> try parseDecal
  <|> try comment
  <?> "Expr"

blendu:: MaterialParser OptionsToken
blendu = parseBool "-blendu" <&> Blendu
blendv :: MaterialParser OptionsToken
blendv = parseBool "-blendv" <&> Blendv
boost :: MaterialParser OptionsToken
boost = parseSingleFloat "-boost" <&> Boost
mm    :: MaterialParser OptionsToken
mm = parseVector2 "-mm" <&> Mm
o     :: MaterialParser OptionsToken
o = parseVector3 "-o" <&> O
s     :: MaterialParser OptionsToken
s = parseVector3 "-s" <&> S
t     :: MaterialParser OptionsToken
t = parseVector3 "-t" <&> T
texres:: MaterialParser OptionsToken
texres = parseSingleFloat "-texres" <&> Texres
clamp :: MaterialParser OptionsToken
clamp = parseBool "-clamp" <&> Clamp
bm    :: MaterialParser OptionsToken
bm = parseSingleFloat "-texres" <&> Bm
imfch :: MaterialParser OptionsToken
imfch = do
  void $ string "-imfchan" <* space
  val <- oneOf "rgbmlz"
  case val of 'r' -> return $ Imfchan R
              'g' -> return $ Imfchan G
              'b' -> return $ Imfchan B
              'm' -> return $ Imfchan M
              'l' -> return $ Imfchan L
              'z' -> return $ Imfchan Z
              _   -> fail "Can't parse not known imfchan"

optionsFactor:: MaterialParser OptionsToken
optionsFactor =
  try blendu
  <|> try blendv
  <|> try boost
  <|> try mm
  <|> try o
  <|> try s
  <|> try t
  <|> try texres
  <|> try clamp
  <|> try bm
  <|> try imfch

writeOptionTokenToOptions:: OptionsToken -> TextureMapOptions -> TextureMapOptions
writeOptionTokenToOptions (Blendu  val) m = m {textureMapOptionsHorizontalBlending = val}
writeOptionTokenToOptions (Blendv  val) m = m {textureMapOptionsVerticalBlending   = val}
writeOptionTokenToOptions (Boost   val) m = m {textureMapOptionsBoost              = val}
writeOptionTokenToOptions (Mm      val) m = m {textureMapOptionsModify             = val}
writeOptionTokenToOptions (O       val) m = m {textureMapOptionsOriginOffset       = val}
writeOptionTokenToOptions (S       val) m = m {textureMapOptionsScale              = val}
writeOptionTokenToOptions (T       val) m = m {textureMapOptionsTurbulence         = val}
writeOptionTokenToOptions (Texres  val) m = m {textureMapOptionsResolution         = val}
writeOptionTokenToOptions (Clamp   val) m = m {textureMapOptionsClamp              = val}
writeOptionTokenToOptions (Bm      val) m = m {textureMapOptionsBumpMultiplier     = val}
writeOptionTokenToOptions (Imfchan val) m = m {textureMapOptionsChannel            = val}

writeTokenToMaterial:: Token -> Material -> Material
writeTokenToMaterial (Newmtl val)   m = m {materialName                    = val}
writeTokenToMaterial (Ka val)       m = m {materialAmbientColor            = val}
writeTokenToMaterial (Kd val)       m = m {materialDiffuseColor            = val}
writeTokenToMaterial (Ks val)       m = m {materialSpecularColor           = val}
writeTokenToMaterial (Ke val)       m = m {materialEmmissive               = val}
writeTokenToMaterial (Ns val)       m = m {materialSpecularHighlights      = val}
writeTokenToMaterial (Ni val)       m = m {materialOpticalDensity          = val}
writeTokenToMaterial (D val)        m = m {materialDissolve                = val}
writeTokenToMaterial (Illum val)    m = m {materialIlluminationMode        = val}
writeTokenToMaterial (MapKa val)    m = m {materialMapAmbientColor         = val}
writeTokenToMaterial (MapKd val)    m = m {materialMapDiffuseColor         = val}
writeTokenToMaterial (MapKs val)    m = m {materialMapSpecularColor        = val}
writeTokenToMaterial (MapNs val)    m = m {materialMapSpecularHighlights   = val}
writeTokenToMaterial (MapNi val)    m = m {materialMapOpticalDensity       = val}
writeTokenToMaterial (MapD val)     m = m {materialMapDissolve             = val}
writeTokenToMaterial (MapBump val)  m = m {materialMapBump                 = val}
writeTokenToMaterial (Disp val)     m = m {materialMapDisplacement         = val}
writeTokenToMaterial (Decal val)    m = m {materialMapDecal                = val}
writeTokenToMaterial (Tf val)       m = m {materialTransmissionFilterColor = val}
writeTokenToMaterial Comment        m = m

-- In file there can be multiple materials
parseSingleMaterial :: MaterialParser Material
parseSingleMaterial = do
  name <- parseMaterialName <* spaces
  exprs <- manyTill (expr <* spaces) (eof <|> void (lookAhead $ string "newmtl"))
  return $ foldr writeTokenToMaterial initMaterial (name:exprs)

topLevel:: MaterialParser [Material]
topLevel = do
  optional $ endBy comment spaces
  sepBy parseSingleMaterial spaces
-- Data type for a single star record

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module StarData
       ( StarData(..)
       , StarDataAbbrev(..)
       , StarDataRec(..)
       , DeepFieldRec(..)
       , positionVec
       , getStarDataAbbrevFromFile
       , writeRecListToFile
       , abbrevFromStarData
       , starDataRecFromStarData
       , starDataRecToJSON
       , deepFieldRecToJSON
       , jsonToStarDataRec
       ) where

-- Vector3 import (for position function etc)
import Data.Vector.V3

-- CSV file parsing imports
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Csv
import qualified Data.Vector as V
import Text.Read

-- JSON imports
import GHC.Generics
import qualified Data.Aeson as AJ

-- Color
import Color

-- Create StarData typeclass so we can support records from
-- different datasets more easily.
class StarData a where
  starID  :: a -> Int
  starX   :: a -> Double
  starY   :: a -> Double
  starZ   :: a -> Double
  starLum :: a -> Double
  starRGB :: a -> Color

data StarDataAbbrev = SDA
                      { sda_sid   :: !Int
                      , sda_x     :: !Double
                      , sda_y     :: !Double
                      , sda_z     :: !Double
                      , sda_lum   :: !Double
                      , sda_rgb   :: !Color
                      }
                    deriving (Generic, Show)

data StarDataRec = SDR
                   { sid   :: !Int
                   , x     :: !Double
                   , y     :: !Double
                   , z     :: !Double
                   , lum   :: !Double
                   , r     :: !Int
                   , g     :: !Int
                   , b     :: !Int
                   }
                 deriving (Generic, Show)

data DeepFieldRec = DFR
                    { dfr_id  :: !Int
                    , dfr_x   :: !Double
                    , dfr_y   :: !Double
                    , dfr_z   :: !Double
                    , dfr_lum :: !Double
                    , dfr_r   :: !Int
                    , dfr_g   :: !Int
                    , dfr_b   :: !Int
                    , dfr_rad :: !Double
                    }
                  deriving (Generic, Show)

-- Make StarDataAbbrev an instance of StarData
instance StarData StarDataAbbrev where
  starID  = sda_sid
  starX   = sda_x
  starY   = sda_y
  starZ   = sda_z
  starLum = sda_lum
  starRGB = sda_rgb

-- Make StarDataAbbrev an instance of StarData
instance StarData StarDataRec where
  starID    = sid
  starX     = x
  starY     = y
  starZ     = z
  starLum   = lum
  starRGB s = (RGB
               (fromIntegral (r s))
               (fromIntegral (g s))
               (fromIntegral (b s)))

instance StarData DeepFieldRec where
  starID    = dfr_id
  starX     = dfr_x
  starY     = dfr_y
  starZ     = dfr_z
  starLum   = dfr_lum
  starRGB s = (RGB
               (fromIntegral (dfr_r s))
               (fromIntegral (dfr_g s))
               (fromIntegral (dfr_b s)))

abbrevFromStarData :: (StarData a) => a -> StarDataAbbrev
abbrevFromStarData star = SDA (starID star)
                          (starX star) (starY star) (starZ star)
                          (starLum star) (starRGB star)

starDataRecFromStarData :: (StarData a) => a -> StarDataRec
starDataRecFromStarData star = (SDR
                                (starID star)
                                (starX star) (starY star) (starZ star)
                                (starLum star)
                                (truncate (colorR (starRGB star)))
                                (truncate (colorG (starRGB star)))
                                (truncate (colorB (starRGB star))))

positionVec :: (StarData a) => a -> Vector3
positionVec star = Vector3 (starX star) (starY star) (starZ star)


instance FromNamedRecord StarDataRec where
  parseNamedRecord r = SDR <$> r .: "id"
                       <*> r .: "x"
                       <*> r .: "y"
                       <*> r .: "z"
                       <*> r .: "lum"
                       <*> r .: "r"
                       <*> r .: "g"
                       <*> r .: "b"

instance ToNamedRecord StarDataRec where
  toNamedRecord (SDR id x y z lum r g b) = namedRecord [
    "id" .= id, "x" .= x, "y" .= y, "z" .= z,
    "lum" .= lum, "r" .= r, "g" .= g, "b" .= b]
instance DefaultOrdered StarDataRec where
  headerOrder _ = header ["id","x","y","z","lum","r","g","b"]


-- Load abbreviated CSV star data from file
getStarDataAbbrevFromFile :: String ->
                             IO (Either String [StarDataAbbrev])
getStarDataAbbrevFromFile fileName = do
  csvData <- BL.readFile fileName
  case decodeByName csvData of
   Left err -> return (Left err)
   Right (_, v) -> let recList  = (V.toList v :: [StarDataRec])
                       starList = map abbrevFromStarData recList
                   in
                    return . Right $ starList

writeRecListToFile :: String -> [StarDataRec] ->
                      IO ()
writeRecListToFile fn sda = do
  let byteString = encodeDefaultOrderedByName sda
  BL.writeFile fn byteString

starDataRecToJSON :: StarDataRec -> C8.ByteString
starDataRecToJSON sda = AJ.encode sda

jsonToStarDataRec :: C8.ByteString -> Maybe StarDataRec
jsonToStarDataRec json =
  (AJ.decode json) :: Maybe StarDataRec

deepFieldRecToJSON :: DeepFieldRec -> C8.ByteString
deepFieldRecToJSON dfr = AJ.encode dfr

instance AJ.ToJSON StarDataRec where
  toEncoding = AJ.genericToEncoding AJ.defaultOptions

instance AJ.FromJSON StarDataRec where
  --Generics!

instance AJ.ToJSON DeepFieldRec where
  toEncoding = AJ.genericToEncoding AJ.defaultOptions

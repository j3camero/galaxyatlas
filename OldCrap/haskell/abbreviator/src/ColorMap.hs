-- Map of star type to color

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ColorMap
       ( ColorMapRec(..)
       , StarDataNew(..)
       , getColorMapDataFromFile
       , getColorMapFromRecs
       , writeStarDataNewListToFile
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
import GHC.Generics

-- Hashmap
import qualified Data.HashMap as HM

data ColorMapRec = ColorMapRec
                   { cmr_type   :: !String
                   , cmr_chromX :: !Double
                   , cmr_chromY :: !Double
                   , cmr_r      :: !Int
                   , cmr_g      :: !Int
                   , cmr_b      :: !Int
                   }
                 deriving (Show)

{- Make StarData an instance of FromNamedRecord so we can
   convert CSV records to StarData instances -}
instance FromNamedRecord ColorMapRec where
  parseNamedRecord r = ColorMapRec <$> r .: "type"
                       <*> r .: "chromaX"
                       <*> r .: "chromaY"
                       <*> r .: "r"
                       <*> r .: "g"
                       <*> r .: "b"

-- Load CSV star data from a file
getColorMapDataFromFile :: String -> IO (Either String [ColorMapRec])
getColorMapDataFromFile fileName = do
  csvData <- BL.readFile fileName
  case decodeByName csvData of
   Left err -> return (Left err)
   Right (_, v) -> return . Right . V.toList $ v

gcmfr :: [ColorMapRec] -> HM.Map String ColorMapRec ->
         HM.Map String ColorMapRec
gcmfr [] cmap = cmap
gcmfr (r:recs) cmap = gcmfr recs (HM.insert (cmr_type r) r cmap)

-- Create map from string values to record type
getColorMapFromRecs :: [ColorMapRec] -> HM.Map String ColorMapRec
getColorMapFromRecs lst =
  gcmfr lst (HM.empty :: HM.Map String ColorMapRec)

data StarDataNew = SDN
                   { id   :: !Int
                   , x   :: !Double
                   , y   :: !Double
                   , z   :: !Double
                   , lum :: !Double
                   , r   :: !Int
                   , g   :: !Int
                   , b   :: !Int
                   }
                 deriving (Generic, Show)

instance ToNamedRecord StarDataNew where
  -- Generics bitches
instance DefaultOrdered StarDataNew where
  headerOrder _ = header ["id","x","y","z","lum","r","g","b"]

writeStarDataNewListToFile :: String -> [StarDataNew] -> IO ()
writeStarDataNewListToFile fn sdn = do
  let byteString = encodeDefaultOrderedByName sdn
  BL.writeFile fn byteString

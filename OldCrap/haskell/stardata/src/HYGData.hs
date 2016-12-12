-- HYG Catalogue Datatype

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module HYGData
       ( HYGData(..)
       , getHYGDataFromFile
       , hygDataToJSON
       , jsonToHYGData
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

import StarData
import Color

data HYGData = HYGData
               { hyg_id           :: Int
               , hyg_hip          :: !(Maybe Int)
               , hyg_hd           :: !(Maybe Int)
               , hyg_hr           :: !(Maybe Int)
               , hyg_gl           :: !(Maybe String)
               , hyg_bf           :: !(Maybe String)
               , hyg_proper       :: !(Maybe String)
               , hyg_ra           :: !(Maybe Double)
               , hyg_dec          :: !(Maybe Double)
               , hyg_dist         :: !(Maybe Double)
               , hyg_pmra         :: !(Maybe Double)
               , hyg_pmdec        :: !(Maybe Double)
               , hyg_rv           :: !(Maybe Double)
               , hyg_mag          :: !(Maybe Double)
               , hyg_absmag       :: !(Maybe Double)
               , hyg_spect        :: !(Maybe String)
               , hyg_ci           :: !(Maybe Double)
               , hyg_x            :: !Double
               , hyg_y            :: !Double
               , hyg_z            :: !Double
               , hyg_vx           :: !(Maybe Double)
               , hyg_vy           :: !(Maybe Double)
               , hyg_vz           :: !(Maybe Double)
               , hyg_rarad        :: !(Maybe Double)
               , hyg_decrad       :: !(Maybe Double)
               , hyg_pmrarad      :: !(Maybe Double)
               , hyg_pmdecrad     :: !(Maybe Double)
               , hyg_bayer        :: !(Maybe String)
               , hyg_flam         :: !(Maybe String)
               , hyg_con          :: !(Maybe String)
               , hyg_comp         :: !(Maybe Int)
               , hyg_comp_primary :: !(Maybe Int)
               , hyg_lum          :: !Double
               , hyg_var          :: !(Maybe String)
               , hyg_var_min      :: !(Maybe Double)
               , hyg_var_max      :: !(Maybe Double)
               }
             deriving (Generic, Show)

-- Make HYGData an instance of StarData
instance StarData HYGData where
  starID  = hyg_id
  starX   = hyg_x
  starY   = hyg_y
  starZ   = hyg_z
  starLum = hyg_lum
  starRGB = \s -> RGB 0 0 0

{- Make StarData an instance of FromNamedRecord so we can
   convert CSV records to StarData instances -}
instance FromNamedRecord HYGData where
  parseNamedRecord r = HYGData <$> r .: "id"
                       <*> r .: "hip"
                       <*> r .: "hd"
                       <*> r .: "hr"
                       <*> r .: "gl"
                       <*> r .: "bf"
                       <*> r .: "proper"
                       <*> r .: "ra"
                       <*> r .: "dec"
                       <*> r .: "dist"
                       <*> r .: "pmra"
                       <*> r .: "pmdec"
                       <*> r .: "rv"
                       <*> r .: "mag"
                       <*> r .: "absmag"
                       <*> r .: "spect"
                       <*> r .: "ci"
                       <*> r .: "x"
                       <*> r .: "y"
                       <*> r .: "z"
                       <*> r .: "vx"
                       <*> r .: "vy"
                       <*> r .: "vz"
                       <*> r .: "rarad"
                       <*> r .: "decrad"
                       <*> r .: "pmrarad"
                       <*> r .: "pmdecrad"
                       <*> r .: "bayer"
                       <*> r .: "flam"
                       <*> r .: "con"
                       <*> r .: "comp"
                       <*> r .: "comp_primary"
                       <*> r .: "lum"
                       <*> r .: "var"
                       <*> r .: "var_min"
                       <*> r .: "var_max"

{- Make StarData an instance of ToJSON so that we can convert
   StarData instances to JSON objects -}
instance AJ.ToJSON HYGData where
  -- Mostly taken care of by generics
  toEncoding = AJ.genericToEncoding AJ.defaultOptions

instance AJ.FromJSON HYGData where
  -- Again, yay generics

-- Load CSV star data from a file
getHYGDataFromFile :: String -> IO (Either String [HYGData])
getHYGDataFromFile fileName = do
  csvData <- BL.readFile fileName
  case decodeByName csvData of
   Left err -> return (Left err)
   Right (_, v) -> return . Right . V.toList $ v

-- Turn a star data record into a JSON object
hygDataToJSON :: HYGData -> C8.ByteString
hygDataToJSON starData = AJ.encode starData

-- Attempt to turn a JSON object into a StarData record
jsonToHYGData :: C8.ByteString -> Maybe HYGData
jsonToHYGData json =
  (AJ.decode json) :: Maybe HYGData


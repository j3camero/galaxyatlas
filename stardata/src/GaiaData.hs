-- HYG Catalogue Datatype

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GaiaData
       ( GaiaData(..)
       , getGaiaDataFromFile
       , gaiaDataToJSON
       , jsonToGaiaData
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

data GaiaData = GaiaData
                { gaia_hip                           :: !(Maybe Int)
                , gaia_tycho2_id                     :: !(Maybe String)
                , gaia_solution_id                   :: !Integer
                , gaia_source_id                     :: !Integer
                , gaia_random_index                  :: !Integer
                , gaia_ref_epoch                     :: !Double
                , gaia_ra                            :: !Double
                , gaia_ra_error                      :: !Double
                , gaia_dec                           :: !Double
                , gaia_dec_error                     :: !Double
                , gaia_parallax                      :: !Double
                , gaia_parallax_error                :: !Double
                , gaia_pmra                          :: !Double
                , gaia_pmra_error                    :: !Double
                , gaia_pmdec                         :: !Double
                , gaia_pmdec_error                   :: !Double
                , gaia_ecl_lon                       :: !Double
                , gaia_ecl_lat                       :: !Double
                }
              deriving (Generic, Show)

-- Make HYGData an instance of StarData
{-
instance StarData HYGData where
  starID  = hyg_id
  starX   = hyg_x
  starY   = hyg_y
  starZ   = hyg_z
  starLum = hyg_lum
  starST  = hyg_spect
-}

{- Make StarData an instance of FromNamedRecord so we can
   convert CSV records to StarData instances -}
instance FromNamedRecord GaiaData where
  parseNamedRecord r = GaiaData <$>
                       r .: "hip"
                       <*> r .: "tycho2_id"
                       <*> r .: "solution_id"
                       <*> r .: "source_id"
                       <*> r .: "random_index"
                       <*> r .: "ref_epoch"
                       <*> r .: "ra"
                       <*> r .: "ra_error"
                       <*> r .: "dec"
                       <*> r .: "dec_error"
                       <*> r .: "parallax"
                       <*> r .: "parallax_error"
                       <*> r .: "pmra"
                       <*> r .: "pmra_error"
                       <*> r .: "pmdec"
                       <*> r .: "pmdec_error"
                       <*> r .: "ecl_lon"
                       <*> r .: "ecl_lat"

{- Make StarData an instance of ToJSON so that we can convert
   StarData instances to JSON objects -}
instance AJ.ToJSON GaiaData where
  -- Mostly taken care of by generics
  toEncoding = AJ.genericToEncoding AJ.defaultOptions

instance AJ.FromJSON GaiaData where
  -- Again, yay generics

-- Load CSV star data from a file
getGaiaDataFromFile :: String -> IO (Either String [GaiaData])
getGaiaDataFromFile fileName = do
  csvData <- BL.readFile fileName
  case decodeByName csvData of
   Left err -> return (Left err)
   Right (_, v) -> return . Right . V.toList $ v

-- Turn a star data record into a JSON object
gaiaDataToJSON :: GaiaData -> C8.ByteString
gaiaDataToJSON starData = AJ.encode starData

-- Attempt to turn a JSON object into a StarData record
jsonToGaiaData :: C8.ByteString -> Maybe GaiaData
jsonToGaiaData json =
  (AJ.decode json) :: Maybe GaiaData


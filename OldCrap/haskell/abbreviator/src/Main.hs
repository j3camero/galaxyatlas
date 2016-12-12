-- Play around with some concepts

import Data.Vector.V3
import Debug.Trace

import qualified Data.HashMap as HM

import qualified StarData as SD
import qualified HYGData as HD
import qualified GaiaData as GD
import ColorMap
import StarTree
import Paths_abbreviator

starFileName = "hygdata_min.csv"
colorMapFileName = "colormap.csv"
out_fileName = "hygdata_min2.csv"

loadStarList :: String -> IO [SD.StarDataAbbrev]
loadStarList fn = do
  dataFilePath <- getDataFileName fn
  dataList <- SD.getStarDataAbbrevFromFile dataFilePath
  case dataList of
   Left err -> do
     putStrLn err
     return []
   Right starData -> return starData

loadColorRecs :: String -> IO [ColorMapRec]
loadColorRecs fn = do
  dataFilePath <- getDataFileName fn
  dataList <- getColorMapDataFromFile dataFilePath
  case dataList of
   Left err -> do
     putStrLn err
     return []
   Right starData -> return starData

oldToNew :: [SD.StarDataAbbrev] -> HM.Map String ColorMapRec ->
            [StarDataNew] -> [StarDataNew]
oldToNew [] cmap acc = acc
oldToNew (x:xs) cmap acc =
  let mSpect = (SD.starST x)
  in
   case mSpect of
    Nothing -> oldToNew xs cmap
                   (acc ++ [(SDN
                             (SD.starID  x)
                             (SD.starX   x)
                             (SD.starY   x)
                             (SD.starZ   x)
                             (SD.starLum x)
                             255
                             128
                             0)])
    (Just spect) -> let mrcrd = HM.lookup spect cmap
                    in
                     case mrcrd of
                      -- Return a default color if lookup fails
                      Nothing     -> oldToNew xs cmap
                                     (acc ++ [(SDN
                                               (SD.starID  x)
                                               (SD.starX   x)
                                               (SD.starY   x)
                                               (SD.starZ   x)
                                               (SD.starLum x)
                                               255
                                               128
                                               0)])
                      -- Return the proper color if lookup success
                      (Just rcrd) -> oldToNew xs cmap
                                     (acc ++ [(SDN
                                               (SD.starID  x)
                                               (SD.starX   x)
                                               (SD.starY   x)
                                               (SD.starZ   x)
                                               (SD.starLum x)
                                               (cmr_r rcrd)
                                               (cmr_g rcrd)
                                               (cmr_b rcrd))])

main = do
  starDataList <- loadStarList starFileName
  starColorRecs <- loadColorRecs colorMapFileName
  let colorMap = getColorMapFromRecs starColorRecs
      newStarDataList = oldToNew starDataList colorMap []
  writeStarDataNewListToFile out_fileName newStarDataList

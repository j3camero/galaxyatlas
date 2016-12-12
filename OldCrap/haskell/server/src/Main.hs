-- Starmap Star Server Driver File

{-# LANGUAGE ScopedTypeVariables #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import StarData
import StarTree
import StarServer
import Paths_starserver

import System.FilePath
import System.Directory

fileName = "hygdata_min2.csv"

main = do
  starDataFilePath <- getDataFileName fileName
  curDir <- getCurrentDirectory
  let staticDir = curDir </> "static"
      scriptDir = staticDir
  putStrLn ("Loading star map from: " ++ starDataFilePath)
  putStrLn ("Serving script files from: " ++ scriptDir)
  starDataList <- getStarDataAbbrevFromFile starDataFilePath
  case starDataList of
   Left err -> putStrLn err
   Right dataList ->
     let starTree = fromList dataList
     in do
       writeJSInterface scriptDir
       run 80 (starServerApp starTree staticDir)

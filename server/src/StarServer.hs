-- Web API part of the code

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module StarServer where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.Except
import Control.Monad.Trans.Either
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import Data.Vector.V3
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.API.ContentTypes
import Servant.JS
import System.Directory
import System.FilePath
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

-- StarData imports
import StarData
import StarTree

-- File to output javascript interface to
ifaceFile :: FilePath
ifaceFile = "StarMap.js"

type StarsAPI =
  "starsInRadius"
  :> QueryParam "radius" Double
  :> QueryParam "pointX" Double
  :> QueryParam "pointY" Double
  :> QueryParam "pointZ" Double
  :> Get '[JSON] [StarDataRec]
  :<|>
  "visibleStars"
  :> QueryParam "minLum" Double
  :> QueryParam "pointX" Double
  :> QueryParam "pointY" Double
  :> QueryParam "pointZ" Double
  :> Get '[JSON] [StarDataRec]
  :<|>
  "visibleStarsMagic"
  :> QueryParam "minLum"  Double
  :> QueryParam "blurRad" Double
  :> QueryParam "pointX"  Double
  :> QueryParam "pointY"  Double
  :> QueryParam "pointZ"  Double
  :> Get '[JSON] [StarDataRec]

type FilesAPI =
  Raw

type StarMapAPI =
    StarsAPI :<|> FilesAPI

starsAPI :: Proxy StarsAPI
starsAPI = Proxy

starMapAPI :: Proxy StarMapAPI
starMapAPI = Proxy

genStarServer :: StarTree StarDataAbbrev -> String -> Server StarMapAPI
genStarServer tree scriptDir = ((starsInRadius tree) :<|>
                                (visStars tree) :<|>
                                (visStarsM tree)) :<|>
                               (serveDirectory scriptDir)

starsInRadius :: StarTree StarDataAbbrev ->
                 Maybe Double ->
                 Maybe Double -> Maybe Double -> Maybe Double ->
                 ExceptT ServantErr IO [StarDataRec]
starsInRadius tree (Just radius) (Just px) (Just py) (Just pz) =
  let starList = inRadius (Vector3 px py pz) radius tree
  in
   return $ map starDataRecFromStarData starList

visStars :: StarTree StarDataAbbrev ->
            Maybe Double ->
            Maybe Double -> Maybe Double -> Maybe Double ->
            ExceptT ServantErr IO [StarDataRec]
visStars tree (Just minLum) (Just px) (Just py) (Just pz) =
  let starList = visibleStars tree (Vector3 px py pz) minLum
  in
   return $ map starDataRecFromStarData starList

visStarsM :: StarTree StarDataAbbrev ->
             Maybe Double ->
             Maybe Double ->
             Maybe Double -> Maybe Double -> Maybe Double ->
             ExceptT ServantErr IO [StarDataRec]
visStarsM
  tree (Just minLum) (Just blurRad) (Just px) (Just py) (Just pz) =
  let starList = visibleStarsMagic tree (Vector3 px py pz) minLum blurRad
  in
   return $ map starDataRecFromStarData starList

writeJSInterface :: FilePath -> IO ()
writeJSInterface scriptDir =
  writeJSForAPI starsAPI vanillaJS (scriptDir </> ifaceFile)

starServerApp :: StarTree StarDataAbbrev -> String -> Application
starServerApp tree staticDir =
  serve starMapAPI (genStarServer tree staticDir)

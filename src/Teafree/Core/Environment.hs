{-

    teafree, a Haskell utility for tea addicts
    Copyright (C) 2013 Fabien Dubosson <fabien.dubosson@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Teafree.Core.Environment
    ( Environment
    , getEnvironment
    , get
    , set
    , modify
    , teas
    , families
    ) where


import Data.Label
import Control.Monad
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

import Paths_teafree
import Teafree.Core.Parsers
import Teafree.Entity.Family as F
import Teafree.Entity.Tea as T


fclabels [d|
    data Environment = Environment
        { teas         :: [Tea]
        , families     :: [Family]
        } deriving (Show)
    |]

defaultEnvironment :: Environment
defaultEnvironment = Environment [] []

getEnvironment :: IO Environment
getEnvironment = do
    fContent <- getOrCopyConfigFileName "families.txt" >>= readFile
    --tContent <- getOrCopyConfigFileName "teas.txt" >>= readFile

    let fParsed = parseFamilies fContent
    --let tParsed = parseTeas tContent

    let fs = case fParsed of
                 Left _ -> []
                 Right xs -> xs

    --let ts = case tParsed of
    --             Left m -> []
    --             Right xs -> xs

    cfs <- mapM correctIcon fs

    return . set families cfs $ defaultEnvironment

correctIcon :: Family -> IO Family
correctIcon f = do
    nIcon <- getDataFileName $ get icon f
    let nFam = set icon nIcon f
    return nFam

getConfigDirectory :: IO FilePath
getConfigDirectory = do
    configHomeVar <- lookupEnv "XDG_CONFIG_HOME"

    let configHome = case configHomeVar of
                   Just v -> v
                   Nothing -> "$HOME/.config"

    let configDir = configHome </> "teafree"

    dirExist <- doesDirectoryExist configDir

    when (not dirExist) $ do
        createDirectory configDir

    return configDir

getConfigFileName :: FilePath -> IO FilePath
getConfigFileName f = do
    configDir <- getConfigDirectory
    return $ configDir </> f

getOrCopyConfigFileName :: FilePath -> IO FilePath
getOrCopyConfigFileName f = do
    file <- getConfigFileName f
    fileExist <- doesFileExist file
    when (not fileExist) $ do
        original <- getDataFileName f
        print original
        copyFile original file
    return file


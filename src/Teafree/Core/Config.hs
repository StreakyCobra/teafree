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

module Teafree.Core.Config where

import Data.Label
import Data.List as DL
import Control.Monad
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

import Paths_teafree
import Teafree.Core.Environment
import Teafree.Core.Monad
import Teafree.Core.Parsers
import Teafree.Entity.Family as F
import Teafree.Entity.Tea as Tea

import Data.Text as T
import Data.Text.IO as TIO
default (T.Text)

getEnvironment :: Teafree Environment
getEnvironment = do
    fContent <- liftIO $ getOrCopyConfigFileName "families.txt" >>= TIO.readFile
    tContent <- liftIO $ getOrCopyConfigFileName "teas.txt" >>= TIO.readFile

    let fParsed = parseFamilies fContent
    let tParsed = parseTeas tContent

    let fs = case fParsed of
                 Left _ -> []
                 Right xs -> xs

    let ts = case tParsed of
                 Left _ -> []
                 Right xs -> xs

    cfs <- mapM correctIcon fs
    cts <- mapM (correctFamily cfs) ts

    return . set teas cts . set families cfs $ defaultEnvironment

correctIcon :: Family -> Teafree Family
correctIcon f = do
    nIcon <- liftIO $ getDataFileName $ T.unpack $ get F.icon f
    let nFam = set F.icon (T.pack nIcon) f
    return nFam

correctFamily :: [Family] -> Tea -> Teafree Tea
correctFamily fs t = do
    case get fam t of
        Left n -> do
            let nFam = DL.find ((==n) . get F.name) fs
            case nFam of
                Nothing -> failure $ "The family \"" ++ (T.unpack n) ++ "\" doesn't exist"
                Just f -> return $ set fam (Right f) t
        Right _ -> return t

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


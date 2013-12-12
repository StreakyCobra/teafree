{-# LANGUAGE TemplateHaskell, TypeOperators #-}

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

import Paths_teafree
import Teafree.Tea as T
import Teafree.Family as F
import Teafree.Units
import Teafree.Parser.Families

fclabels [d|
    data Environment = Environment
        { teas         :: [Tea]
        , families     :: [Family]
        } deriving (Show)
    |]

getEnvironment :: IO Environment
getEnvironment = do
    content <- getDataFileName "families.txt" >>= readFile

    let fams = parseFamilies content
    case fams of
        Left m -> return defaultEnvironment
        Right fs -> do
            cfs <- mapM ci fs
            let env = set families cfs $ defaultEnvironment
            return env
    where ci f = do
            v <- getDataFileName $ get F.icon f
            let u = set F.icon v f
            return u

defaultEnvironment :: Environment
defaultEnvironment = Environment [] []


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

import Paths_teafree
import Teafree.Tea
import Teafree.Family

fclabels [d|
    data Environment = Environment
        { teas       :: [Tea]
        , families   :: [Family]
        } deriving (Show)
    |]

getEnvironment :: IO Environment
getEnvironment = do
    icon <- getDataFileName "images/mate.png"
    icon2 <- getDataFileName "images/oolang.png"
    let afam = [Family "Mate" icon 2 100 1 20, Family "Oolang" icon2 1 95 2 19]
    let env = set families afam $ defaultEnvironment
    return env
    where


defaultEnvironment :: Environment
defaultEnvironment = Environment [] []

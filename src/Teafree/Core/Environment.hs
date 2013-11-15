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

module Teafree.Core.Environment where

import Data.Label

import Teafree.Tea
import Teafree.Family

data Environment = Environment
    { _teas       :: [Tea]
    , _families   :: [Family]
    } deriving (Show)

mkLabel ''Environment

getEnvironment :: IO Environment
getEnvironment = return defaultEnvironment

defaultEnvironment :: Environment
defaultEnvironment = Environment
    { _teas = []
    , _families = []
    }

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
    , defaultEnvironment
    , get
    , set
    , modify
    , teas
    , families
    ) where


import Data.Label
import Data.List as DL
import Control.Monad
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

import Paths_teafree
import Teafree.Core.Parsers
import Teafree.Entity.Family as F
import Teafree.Entity.Tea as Tea

import Data.Text as T
import Data.Text.IO as TIO
default (T.Text)


fclabels [d|
    data Environment = Environment
        { teas         :: [Tea]
        , families     :: [Family]
        } deriving (Show)
    |]

defaultEnvironment :: Environment
defaultEnvironment = Environment [] []


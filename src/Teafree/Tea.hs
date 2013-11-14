{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}

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

module Teafree.Tea where

import Data.Label
import Text.Printf

import Teafree.Category
import Teafree.Interaction.Format
import Teafree.Units

data Tea = Tea
    { _name        :: String
    , _category    :: Category
    , _quantity    :: Quantity
    , _temperature :: Temperature
    , _time        :: Time
    , _cafeine     :: Percentage
    } deriving (Show)

mkLabel ''Tea

instance ToDoc Tea where
    toDoc f t = "Tea {"


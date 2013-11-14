{-# LANGUAGE OverloadedStrings #-}

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

module Teafree.Interaction.Choice where

import Control.Monad
import Control.Exception
import Data.Text as T
import Shelly

default (T.Text)

chooseTea :: Sh Text
chooseTea = listOfTeas -|- chooser
    where listOfTeas :: Sh Text
          listOfTeas = liftM T.unlines $ lsT "."

chooser :: Sh Text
chooser = catch_sh
            (run "dmenu" ["-i", "-p", "Tea:", "-l", "10"])
            ((\_ -> return "") :: SomeException -> Sh Text)

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

module Teafree.Interaction.Choice
    ( chooseTea
    , chooseFamily
    , chooser
    ) where

import Prelude as P
import Control.Monad
import Data.Text as T
import Shelly

import Teafree.Core.PPrint
import Teafree.Core.Monad
import Teafree.Core.Environment as E
import Teafree.Family

import Control.Exception
default (T.Text)

chooseTea :: Teafree String
chooseTea = do
        env <- ask
        let listOfTeas = listToText $ E.get teas env
        choice <- shellyNoDir $ silently $ print_stdout False $ do
                    return listOfTeas -|- chooser
        return . T.unpack $ choice

chooseFamily :: Teafree String
chooseFamily = do
        env <- ask
        let listOfFamilies = listToText $ E.get families env
        choice <- shellyNoDir $ silently $ print_stdout False $ do
                    return listOfFamilies -|- chooser
        return . T.unpack $ choice

chooser :: Sh Text
chooser = catch_sh
            (run "dmenu" ["-i", "-p", "teafree:", "-l", "10"])
            ((\_ -> return "") :: SomeException -> Sh Text)

listToText :: (PPrint a) => [a] -> Text
listToText = T.unlines . P.map (T.pack . show . ppName False)

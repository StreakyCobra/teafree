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

{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Teafree.Interaction.Choice
    ( chooseTea
    , chooseFamily
    , forName
    ) where


import Control.Exception
import Data.Label
import Data.List as DL
import Shelly hiding (get)

import Teafree.Core.Monad
import Teafree.Core.Environment

import Teafree.Entity.Family as Fam
import Teafree.Entity.Tea as Tea

import Data.Text as T hiding (map)
default (T.Text)


chooseTea :: Teafree (Maybe Tea)
chooseTea = chooseItem teas (get Tea.name)

chooseFamily :: Teafree (Maybe Family)
chooseFamily = chooseItem families (get Fam.name)

chooseItem :: (Environment :-> [a]) -> (a -> String) -> Teafree (Maybe a)
chooseItem a f = do
    env <- ask
    choice <- chooseText . listToText f $ get a env
    case choice of
        "" -> return Nothing
        _ -> forName a f $ DL.init . T.unpack $ choice

chooseText :: Text -> Teafree Text
chooseText t = shellyNoDir $ silently $ print_stdout False $ return t -|- chooser

chooser :: Sh Text
chooser = catch_sh
            (run "dmenu" ["-i", "-p", "teafree:", "-l", "10"])
            ((\_ -> return "") :: SomeException -> Sh Text)

forName :: (Environment :-> [a]) -> (a -> String) -> String -> Teafree (Maybe a)
forName a f s = do
    env <- ask
    return . DL.find ((==s) . f) $ get a env

listToText :: (a -> String) -> [a] -> Text
listToText f = T.unlines . map (T.pack . f)


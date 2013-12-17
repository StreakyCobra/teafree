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


import Data.Label
import Data.List as DL
import Shelly hiding (get)

import Teafree.Core.Monad
import Teafree.Core.Environment

import qualified Teafree.Entity.Family as Fam
import qualified Teafree.Entity.Tea as Tea

import Data.Text as T hiding (map)
default (T.Text)


chooseTea :: Teafree Tea.Tea
chooseTea = chooseItem teas (get Tea.name)

chooseFamily :: Teafree Fam.Family
chooseFamily = chooseItem families (get Fam.name)

chooseItem :: (Environment :-> [a]) -> (a -> Text) -> Teafree a
chooseItem a f = do
    env <- ask
    choice <- chooseText . listToText f $ get a env
    case choice of
        "" -> abort
        _ -> forName a f $ T.init choice

chooseText :: Text -> Teafree Text
chooseText t = liftIO $ catchany (shellyNoDir $ silently $ print_stdout False $ return t -|- chooser)
                                 (\_ -> return "")

chooser :: Sh Text
chooser = run "dmenu" ["-i", "-p", "teafree:", "-l", "10"]

forName :: (Environment :-> [a]) -> (a -> Text) -> Text -> Teafree a
forName a f s = do
    env <- ask
    let result = DL.find ((==s) . f) $ get a env
    case result of
        Nothing -> failure $ "The item with name \"" ++ T.unpack s ++ "\" doesn't exist"
        Just v -> return v

listToText :: (a -> Text) -> [a] -> Text
listToText f = T.unlines . map f


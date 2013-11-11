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

module Teafree.Command.Prepare
    ( prepare
    ) where

import Shelly
import Control.Monad
import Control.Concurrent
import Data.Text as T
default (T.Text)

{- Prepare a tea -}
prepare :: IO ()
prepare = shellyNoDir $ print_stdout False $ do
    choice <- (liftM T.unlines listTeas) -|- chooser
    teaTime <- return $ 2
    liftIO . threadDelay . (*1000000) $ teaTime
    notify choice

{- List all teas -}
listTeas :: Sh [Text]
listTeas = lsT "/home/fabien"

{- Permit to choose an element from STDIN -}
chooser :: Sh Text
chooser = run "dmenu" []

notify :: Text -> Sh ()
notify choice = run_ "notify-send" ["Test", choice]
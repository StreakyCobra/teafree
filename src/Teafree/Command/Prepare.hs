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

import Control.Concurrent
import Data.Text as T
import Shelly hiding (get)

import Paths_teafree
import Teafree.Core.Environment
import Teafree.Core.Monad
import Teafree.Interaction.Notify
import Teafree.Interaction.Choice

default (T.Text)

{- Prepare a tea -}
prepare :: Teafree ()
prepare = do
    content <- ask
    choice <- chooseTea

    let (_:f:_) = get teas content

    teaTime <- return $ 2
    testIcon <- liftIO $ getDataFileName "images/oolang.png"
    liftIO . threadDelay . (*1000000) $ teaTime

    shellyNoDir $ silently $ print_stdout False $ do
        send $ def title "Your tea is ready"
             . def body choice
             . def icon (T.pack testIcon)
             . def duration 0
             $ notification

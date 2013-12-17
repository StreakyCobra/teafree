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

{-# LANGUAGE OverloadedStrings #-}

module Teafree.Command.Prepare
    ( prepare
    ) where


import Control.Concurrent
import Shelly hiding (get)

import Teafree.Core.Monad
import Teafree.Interaction.PPrint
import Teafree.Interaction.Choice
import Teafree.Interaction.Notify
import qualified Teafree.Entity.Tea as Tea
import Teafree.Entity.Units

import Data.Text as T
default (T.Text)


{- Prepare a tea -}
prepare :: Teafree ()
prepare = do
    choice <- chooseTea

    liftIO . threadDelay . (*1000000) . toSeconds $ Tea.time choice
    send $ def title (T.pack . show . ppName False $ choice)
         . def body ("Your tea is ready")
         . def icon (Tea.icon choice)
         . def duration 0
         . def urgency "critical"
         $ notification


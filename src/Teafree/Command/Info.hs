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

module Teafree.Command.Info
    ( info
    ) where


import Prelude as P
import Shelly hiding (get)
import Data.Text as T
import Data.List as DL

import Paths_teafree
import Teafree.Core.PPrint
import Teafree.Core.Environment
import Teafree.Core.Monad
import Teafree.Interaction.Notify as N
import Teafree.Interaction.Choice
import Teafree.Family as F

default (T.Text)

{- Prepare a tea -}
info :: Teafree ()
info = do
    choice <- chooseFamily

    case choice of
        Nothing -> sendError "The selected item is not found"
        Just f -> send $ def title (T.pack . show . ppName False $ f)
                      . def body (T.pack . show . ppDetails False $ f)
                      . def N.icon (T.pack $ get F.icon f)
                      . def duration 0
                      . def urgency "normal"
                      $ notification



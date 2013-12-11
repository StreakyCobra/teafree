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

module Teafree.Core.Utils where

import Teafree.Core.Environment
import Teafree.Core.Monad
import Teafree.Tea as T
import Teafree.Family as F

import Control.Monad
import Data.List

familyForName :: String -> Teafree (Maybe Family)
familyForName n = do
    env <- ask
    when (length n == 0) $ failure "You must specify one item"
    let query = head . words $ n
    return . find ((==query) . get F.name) $ get families env

teaForName :: String -> Teafree (Maybe Tea)
teaForName n = do
    env <- ask
    when (length n == 0) $ failure "You must specify one item"
    let query = head . words $ n
    return . find ((==query) . get T.name) $ get teas env

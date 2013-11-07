{-# LANGUAGE DeriveDataTypeable #-}

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

module Main where

import System.Console.CmdArgs

import qualified Teafree.Version as V
import qualified Teafree.Command.Tea as T

{- Teafree available modes -}
data TeafreeMode = Teas
        deriving ( Data, Typeable, Show, Eq)

{- Teas mode -}
teas :: TeafreeMode
teas = Teas

{- Teafree mode -}
mode :: Mode (CmdArgs TeafreeMode)
mode = cmdArgsMode $ modes [teas]
    &= program "teafree"
    &= summary "A Haskell utility for tea addicts"
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary V.release]

{- Entry point -}
main :: IO ()
main = cmdArgsRun mode >>= runMode

{- Run a specific mode -}
runMode :: TeafreeMode -> IO ()
runMode Teas = T.printList

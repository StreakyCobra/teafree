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
import System.Exit (exitSuccess, exitFailure)

import Teafree.Core.Environment
import Teafree.Core.Monad
import Teafree.Core.TeafreeError
import Teafree.Core.Version
import qualified Teafree.Command.List as CL
import qualified Teafree.Command.Prepare as CP

{- Teafree available modes -}
data TeafreeMode = List {what :: String}
                 | Prepare
        deriving ( Data, Typeable, Show, Eq)

{- Mode to list items -}
list :: TeafreeMode
list = List {what = def &= opt "teas" &= typ "WHAT" &= argPos 0}
    &= help "List items, where 'WHAT' is either 'teas' or 'categories'"

{- Mode to prepare a tea -}
prepare :: TeafreeMode
prepare = Prepare
    &= help "Ask the user for a tea and time it."

{- One mode to rule them all,
   One mode to find them,
   One mode to bring them all
   and in the darkness bind them -}
teafree :: Mode (CmdArgs TeafreeMode)
teafree = cmdArgsMode $ modes [list, prepare]
    &= program "teafree"
    &= summary "A Haskell utility for tea addicts"
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary release]

{- Run a specific mode -}
runMode :: TeafreeMode -> Teafree ()
runMode (List w) = CL.printList w
runMode Prepare = CP.prepare

{- Entry point of the application, in case of you don't already know -}
main :: IO ()
main = do
    m <- cmdArgsRun teafree
    runTeafree (runMode m) defaultEnvironment >>= end

{- End the program properly, by verifying error messages -}
end :: Either TeafreeError () -> IO ()
end (Left e) = putStrLn (getErrorMsg e) >> exitFailure
end (Right _) = exitSuccess

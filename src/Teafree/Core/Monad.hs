{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Teafree.Core.Monad
    ( Teafree
    , runTeafree
    , liftIO
    , ask
    , failure
    , return
    ) where

import Control.Monad.Reader
import Control.Monad.Error

import Teafree.Core.Environment
import Teafree.Core.TeafreeError

newtype Teafree t = T {runT :: ErrorT TeafreeError (ReaderT Environment IO) t}
    deriving (Monad, MonadError TeafreeError, MonadReader Environment,
             MonadIO)

runTeafree :: Teafree t -> Environment -> IO (Either TeafreeError t)
runTeafree t = runReaderT $ runErrorT (runT t)

failure :: String -> Teafree t
failure = throwError . strMsg


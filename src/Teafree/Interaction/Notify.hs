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

module Teafree.Interaction.Notify
    ( notification
    , addIcon
    , addUrgency
    , send
    ) where

import Data.Text as T
import Shelly

default (T.Text)

data Notification = N
    { duration :: Int
    , title    :: Text
    , body     :: Text
    , icon     :: Maybe Text
    , urgency  :: Maybe Text
    }

notification :: Int -> Text -> Text -> Notification
notification t h b = N
    { duration = t
    , title    = h
    , body     = b
    , icon     = Nothing
    , urgency  = Nothing
    }

addIcon :: Text -> Notification -> Notification
addIcon i n = n { icon = Just i }

addUrgency :: Text -> Notification -> Notification
addUrgency u n = n { urgency = Just u }

send :: Notification -> Sh ()
send n = notifySend $ d ++ u ++ i ++ t ++ b
    where d = ["-t", T.pack . show . (* 1000) . duration $ n]
          t = [title n]
          b = [body n]
          i = case icon n of
                  Nothing -> []
                  Just v -> ["-i", v]
          u = case urgency n of
                  Nothing -> []
                  Just v -> ["-u", v]

notifySend :: [Text] -> Sh ()
notifySend = run_ "notify-send"


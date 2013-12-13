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

{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}

module Teafree.Interaction.Notify
    ( send
    , notification
    , def
    , duration
    , title
    , body
    , icon
    , urgency
    , category
    , sendError
    ) where


import Data.Label
import Prelude as P
import Shelly hiding (get)

import Teafree.Core.Monad

import Data.Text as T
default (T.Text)


fclabels [d|
    data Notification = N
        { duration :: Maybe Int
        , title    :: Maybe Text
        , body     :: Maybe Text
        , icon     :: Maybe Text
        , urgency  :: Maybe Text
        , category :: Maybe Text
        }
    |]

def :: (f :-> Maybe a) -> a -> f -> f
def f v = set f $ Just v

notification :: Notification
notification = N Nothing Nothing Nothing Nothing Nothing Nothing

notifySend :: [Text] -> Sh ()
notifySend = run_ "notify-send"

send :: Notification -> Teafree ()
send n = shellyNoDir $ silently $ print_stdout False $ notifySend . P.concat $ [d, c, u, i, t, b]
    where d = case get duration n of
                  Nothing -> []
                  Just v -> ["-t", T.pack . show . (* 1000) $ v]
          t = case get title n of
                  Nothing -> [""]
                  Just v -> [v]
          b = case get body  n of
                  Nothing -> [""]
                  Just v -> [v]
          i = case get icon n of
                  Nothing -> []
                  Just v -> ["-i", v]
          u = case get urgency n of
                  Nothing -> []
                  Just v -> ["-u", v]
          c = case get category n of
                  Nothing -> []
                  Just v -> ["-c", v]

sendError :: Text -> Teafree ()
sendError m = send $ def title "Error"
               . def body m
               . def icon "dialog-warning"
               . def duration 5
               $ notification


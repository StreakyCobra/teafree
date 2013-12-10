{-# LANGUAGE TemplateHaskell, TypeOperators #-}

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

module Teafree.Family where

import Data.Label
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Teafree.Units

fclabels [d|
    data Family = Family
        { name        :: String
        , icon        :: String
        , quantity    :: Quantity
        , temperature :: Temperature
        , time        :: Time
        , cafeine     :: Maybe Percentage
        }
    |]

instance Show Family where
    show t = show $
                (bold . dullred $ (text . get name $ t)) <$>
                indent 4 (
                    text (printf "%-15s" "Quantity:") <+> text (show $ get quantity t) <$>
                    text (printf "%-15s" "Temperature:") <+> text (show $ get temperature t) <$>
                    text (printf "%-15s" "Time:") <+> text (show $ get time t) <$>
                    text (printf "%-15s" "Cafeine:") <+>
                        case (get cafeine t) of
                            Just v ->  text (show v) <+> text "of a coffee"
                            Nothing ->  yellow (text "Unknown")
                    ) <$>
                    empty

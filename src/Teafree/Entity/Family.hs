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

{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Teafree.Entity.Family where


import Data.Label
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen as PP

import Teafree.Interaction.PPrint
import Teafree.Entity.Units

import Data.Text as T
default (T.Text)


fclabels [d|
    data Family = Family
        { name        :: Text
        , icon        :: Text
        , quantity    :: Quantity
        , temperature :: Temperature
        , time        :: Time
        , cafeine     :: Maybe Percentage
        }
    |]

instance Show Family where
    show = show . pprint False

instance PPrint Family where
    ppName c v = i (bold . dullred) $ text . T.unpack . get name $ v
            where i f = if c then f else id

    ppDetails c v = text (printf "%-15s" "Quantity:") <+> (pprint c $ get quantity v) <$>
                    text (printf "%-15s" "Temperature:") <+> (pprint c $ get temperature v) <$>
                    text (printf "%-15s" "Time:") <+> (pprint c $ get time v) <$>
                    text (printf "%-15s" "Cafeine:") <+>
                        case (get cafeine v) of
                            Just t -> (pprint c t) <+> text "of coffee"
                            Nothing -> (i yellow) (text "Unknown")
                    <$> PP.empty
            where i f = if c then f else id

    ppSummary c v = text (T.unpack $ get name v) <+> text " (" <>
                    (pprint c $ get quantity v) <> text " | " <>
                    (pprint c $ get temperature v) <> text " | " <>
                    (pprint c $ get time v) <>
                    (case (get cafeine v) of
                        Just t -> text " | " <> pprint c t <+> text "of coffee"
                        Nothing -> text "") <>
                    text ")"
            where i f = if c then f else id


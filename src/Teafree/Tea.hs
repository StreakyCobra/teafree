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

module Teafree.Tea where

import Data.Label
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Teafree.Core.Classes
import qualified Teafree.Family as F
import Teafree.Units

fclabels [d|
    data Tea = Tea
        { name        :: String
        , family      :: F.Family
        , quantity    :: Maybe Quantity
        , temperature :: Maybe Temperature
        , time        :: Maybe Time
        , cafeine     :: Maybe Percentage
        }
    |]

instance Show Tea where
    show t = show $
                (bold . dullred $ (text . get name $ t)) <$>
                indent 4 (
                    text (printf "%-15s" "Quantity:") <+> text (show $ get quantity t) <$>
                    text (printf "%-15s" "Temperature:") <+> text (show $ get temperature t) <$>
                    text (printf "%-15s" "Time:") <+> text (show $ get time t) <$>
                    text (printf "%-15s" "Cafeine:") <+> text (show $ get cafeine t) <+> text "of a coffee"
                    ) <$>
                    empty

instance PPrint Tea where
    pprint t = show $
                (bold . dullred $ (text . get name $ t)) <$>
                indent 4 (
                    text (printf "%-15s" "Quantity:") <+> text (pprint $ get quantity t) <$>
                    text (printf "%-15s" "Temperature:") <+> text (pprint $ get temperature t) <$>
                    text (printf "%-15s" "Time:") <+> text (pprint $ get time t) <$>
                    text (printf "%-15s" "Cafeine:") <+> text (pprint $ get cafeine t) <+> text "of a coffee"
                    ) <$>
                    empty

instance Summary Tea where
    summary f = printf "%s (%s | %s | %s%s)"
                    (get name f)
                    (show $ get quantity f)
                    (show $ get temperature f)
                    (show $ get time f)
                    (show $ case (get cafeine f) of
                        Just (Percent v) -> text " | " <> text (show v) <+> text "of a coffee"
                        Just Free -> text " | " <> text "Cafeine free"
                        Nothing -> text "")

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

import Teafree.Core.PPrint
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
    show = show . pprint False

instance PPrint Tea where
    ppName c v = i (bold . dullred) $ text . get name $ v
            where i f = if c then f else id

    ppDetails c t = text (printf "%-15s" "Quantity:") <+> (pprint c $ get quantity t) <$>
                    text (printf "%-15s" "Temperature:") <+> (pprint c $ get temperature t) <$>
                    text (printf "%-15s" "Time:") <+> (pprint c $ get time t) <$>
                    text (printf "%-15s" "Cafeine:") <+>
                        case (get cafeine t) of
                            Just Free -> i dullblue $ text "Cafeine free"
                            Just v -> pprint c v <+> text "of a coffee"
                            Nothing -> i yellow $ text "Unknown"
            where i f = if c then f else id

    ppSummary c f = text $ printf "%s (%s | %s | %s%s)"
                            (get name f)
                            (show $ get quantity f)
                            (show $ get temperature f)
                            (show $ get time f)
                            (show $ case (get cafeine f) of
                                Just (Percent v) -> text " | " <> text (show v) <+> text "of a coffee"
                                Just Free -> text " | " <> text "Cafeine free"
                                Nothing -> text "")

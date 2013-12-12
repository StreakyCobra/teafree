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

module Teafree.Entity.Units
    ( Temperature (..)
    , Percentage (..)
    , Quantity (..)
    , Time (..)
    , toSeconds
    ) where


import Teafree.Interaction.PPrint

import Text.PrettyPrint.ANSI.Leijen


data Temperature = Celsius Int
                 | Fahrenheit Int

data Percentage = Percent Int
                | Free

newtype Quantity = Tsp Double

newtype Time = Second Int


instance Show Temperature where
    show = show . pprint False

instance Show Quantity where
    show = show . pprint False

instance Show Time where
    show = show . pprint False

instance Show Percentage where
    show = show . pprint False


instance PPrint Temperature where
    pprint c (Celsius v) = (i (bold . dullblue) . text . show $ v) <+> (text "°C")
            where i f = if c then f else id

    pprint c (Fahrenheit v) = (i (bold . dullblue) . text . show $ v) <+> (text "°F")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

instance PPrint Quantity where
    pprint c (Tsp v) = (i (bold . dullblue) . text . show $ v) <+> (text "tsp.")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

instance PPrint Time where
    pprint c (Second v) = (i (bold . dullblue) . text . show $ v) <+> (text "s.")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

instance PPrint Percentage where
    pprint c (Percent v) = (i (bold . dullblue) . text . show $ v) <+> (text "%")
            where i f = if c then f else id

    pprint c Free = i (bold . dullblue) $ text "Free"
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

toSeconds :: Time -> Int
toSeconds (Second s) = s


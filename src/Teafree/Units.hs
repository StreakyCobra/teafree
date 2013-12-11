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

module Teafree.Units where

import Text.PrettyPrint.ANSI.Leijen
import Teafree.Core.Classes

data Temperature = Celsius Int
                 | Fahrenheit Int

newtype Quantity = Tsp Double

newtype Time = Second Int

data Percentage = Percent Int
                | Free

instance Show Temperature where
    show (Celsius v) = show $
                (text . show $ v) <+>
                (text "°C")
    show (Fahrenheit v) = show $
                (text . show $ v) <+>
                (text "°F")

instance PPrint Temperature where
    pprint (Celsius v) = show $
                (dullblue . text . show $ v) <+>
                (text "°C")
    pprint (Fahrenheit v) = show $
                (dullblue . text . show $ v) <+>
                (text "°F")

instance Show Quantity where
    show (Tsp v) = show $
                (text . show $ v) <+>
                (text "tsp.")

instance PPrint Quantity where
    pprint (Tsp v) = show $
                (dullblue . text . show $ v) <+>
                (text "tsp.")

instance Show Time where
    show (Second v) = show $
                (text . show $ v) <+>
                (text "s.")

instance PPrint Time where
    pprint (Second v) = show $
                (dullblue . text . show $ v) <+>
                (text "s.")

instance Show Percentage where
    show (Percent v) = show $
                (text . show $ v) <+>
                (text "%")
    show Free = show $ text "Free"

instance PPrint Percentage where
    pprint (Percent v) = show $
                (dullblue . text . show $ v) <+>
                (text "%")
    pprint Free = show $ dullblue $ text "Free"

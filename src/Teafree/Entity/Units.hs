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
    , toDl
    , toOz
    , toC
    , toF
    ) where

import Data.Ratio

import Teafree.Interaction.PPrint

import Text.PrettyPrint.ANSI.Leijen


data Temperature = Celsius Int
                 | Fahrenheit Int

data Quantity = TspDl Double
              | TspOz Double

newtype Time = Second Int

newtype Percentage = Percent Int


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
    pprint c (TspDl v) = (i (bold . dullblue) . text . show $ v) <+> (text "tsp/dl")
            where i f = if c then f else id

    pprint c (TspOz v) = (i (bold . dullblue) . text . show $ v) <+> (text "tsp/8oz")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

instance PPrint Time where
    pprint c (Second v) = let (h, hr) = quotRem v 3600 in
                          let (m, s) = quotRem hr 60 in
                          case h of
                              0 -> empty
                              p -> (i (bold . dullblue) . text . show $ p) <+> (text "h ")
                          <> case m of
                              0 -> empty
                              p -> (i (bold . dullblue) . text . show $ p) <+> (text "m ")
                          <> case s of
                              0 -> empty
                              p -> (i (bold . dullblue) . text . show $ p) <+> (text "s")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

instance PPrint Percentage where
    pprint c (Percent 0) = i (bold . dullblue) $ text "Free"
            where i f = if c then f else id

    pprint c (Percent v) = (i (bold . dullblue) . text . show $ v) <+> (text "%")
            where i f = if c then f else id

    ppName = pprint
    ppDetails = pprint
    ppSummary = pprint

toSeconds :: Time -> Int
toSeconds (Second s) = s

toDl :: Quantity -> Quantity
toDl (TspDl v) = TspDl v
toDl (TspOz v) = TspDl v

toOz :: Quantity -> Quantity
toOz (TspOz v) = TspOz v
toOz (TspDl v) = TspOz v

toF :: Temperature -> Temperature
toF (Fahrenheit v) = Fahrenheit v
toF (Celsius v) = Fahrenheit . roundTo 5 . round $ (fromIntegral v) * (9.0 / 5.0) + 32.0

toC :: Temperature -> Temperature
toC (Celsius v) = Celsius v
toC (Fahrenheit v) = Celsius . roundTo 5 . round $ ((fromIntegral v) - 32) * (5.0 / 9.0)

roundTo :: Int -> Int -> Int
roundTo a v = a * q + u
    where (q, r) = quotRem v a
          u = if (fromIntegral r) > ((fromIntegral a) / 2) then a else 0

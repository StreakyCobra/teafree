{-

    teafree, a Haskell utility for tea addicts
    Copyright (C) 2013 Fabien Dubosson <fabien.dubosson@gmail.com>

    This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Teafree.Core.Parsers where


import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), liftA)

import Teafree.Entity.Family
import Teafree.Entity.Units


parseFamilies :: String -> Either ParseError [Family]
parseFamilies = parse famFile ""

famFile = many fam <* eof

fam = do
    name <- field "Name" anyChar
    icon <- field "Icon" anyChar
    quantity <- field "Quantity" pQuantity
    temperature <- field "Temperature" pTemperature
    time <- field "Time" pTime
    cafeine <- field "Cafeine" pPercentage
    many newline
    let aFamily = Family name icon (head quantity) (head temperature) (head time) (head cafeine)
    return aFamily

field s r = string s *> spaces *> string ":" *> spaces *> manyTill r (try newline)

pQuantity = do
    value <- many (noneOf " ") <* spaces <* string "tsp"
    return . Tsp . read $ value

pTemperature = pCelsius <|> pFahrenheit
    where pCelsius = do
              value <- many digit <* spaces <* string "C"
              return . Celsius . read $ value
          pFahrenheit = do
              value <- many digit <* spaces <* string "F"
              return . Fahrenheit . read $ value

pTime = do
    value <- many digit <* spaces <* string "s."
    return . Second . read $ value

pPercentage = do
    value <- many digit <* spaces <* string "%"
    return . Just . Percent . read $ value


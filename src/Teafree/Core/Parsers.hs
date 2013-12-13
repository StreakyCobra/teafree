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
import Control.Applicative ((<*), (*>))

import qualified Teafree.Entity.Family as F
import qualified Teafree.Entity.Tea as T
import qualified Teafree.Entity.Units as U

parseTeas :: String -> Either ParseError [T.Tea]
parseTeas = parse pTeasFile ""

pTeasFile = many pTea <* eof

pTea = do
    _ <- many (softNewline <|> pComment)
    name <- pField "Name" (many $ noneOf "\n#")
    fam <- pField "Family" (many $ noneOf "\n#")
    production <- optionMaybe . try $ pField "Production" (many $ noneOf "\n#")
    quantity <- optionMaybe . try $ pField "Quantity" pQuantity
    temperature <- optionMaybe . try $ pField "Temperature" pTemperature
    time <- optionMaybe . try $ pField "Time" pTime
    cafeine <- optionMaybe . try $ pField "Cafeine" pPercentage
    _ <- many (softNewline <|> pComment)

    let aTea = T.Tea name (Left fam) production quantity temperature time cafeine

    return aTea

parseFamilies :: String -> Either ParseError [F.Family]
parseFamilies = parse pFamiliesFile ""

pFamiliesFile = many pFamily <* eof

pFamily = do
    _ <- many (softNewline <|> pComment)
    name <- pField "Name" (many $ noneOf "\n#")
    _ <- many (softNewline <|> pComment)
    icon <- pField "Icon" (many $ noneOf "\n#")
    _ <- many (softNewline <|> pComment)
    quantity <- pField "Quantity" pQuantity
    _ <- many (softNewline <|> pComment)
    temperature <- pField "Temperature" pTemperature
    _ <- many (softNewline <|> pComment)
    time <- pField "Time" pTime
    _ <- many (softNewline <|> pComment)
    cafeine <- optionMaybe $ pField "Cafeine" pPercentage
    _ <- many (softNewline <|> pComment)

    let aFamily = F.Family name icon quantity temperature time cafeine

    return aFamily

pField s r = string s *> spaces *> string ":" *> spaces *> r <* (try pComment <|> softNewline)

pComment = spaces *> char '#' <* manyTill anyChar (try newline)

pInt = do
    int <- many digit
    return (read int :: Int)

pDouble = do
    int <- many digit
    prec <- optionMaybe (char '.' *> digit <* many digit)
    let val = int ++ case prec of
                         Nothing -> []
                         Just v -> '.':[v]
    return (read val :: Double)

pQuantity = choice [try pTspDl, try pTspOz]
    where pTspDl = do
                value <- pDouble <* spaces <* string "tsp/dl" <* optional (char '.')
                return . U.TspDl $ value
          pTspOz = do
                value <- pDouble <* spaces <* string "tsp/8oz" <* optional (char '.')
                return . U.TspOz $ value

pTemperature = choice [try pCelsius, try pFahrenheit]
    where pCelsius = do
              value <- pInt <* spaces <* string "°C"
              return . U.Celsius $ value
          pFahrenheit = do
              value <- pInt <* spaces <* string "°F"
              return . U.Fahrenheit $ value

pTime = do
    value <- pInt <* spaces <* string "s" <* optional (char '.')
    return . U.Second $ value

pPercentage = do
    value <- pInt <* spaces <* char '%'
    return . U.Percent $ value

softNewline = many (oneOf " \t") >> newline

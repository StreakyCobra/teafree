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


import Control.Applicative ((<*), (*>), (<*>), (<$>), pure)
import Text.ParserCombinators.Parsec

import qualified Teafree.Entity.Family as F
import qualified Teafree.Entity.Tea as T
import qualified Teafree.Entity.Units as U


parseTeas :: String -> Either ParseError [T.Tea]
parseTeas = parse pTeasFile ""

pTeasFile :: Parser [T.Tea]
pTeasFile = many pTea <* eof

pTea :: Parser T.Tea
pTea = do
    name <- pField "Name" pString
    fam <- pField "Family" pString
    production <- optionMaybe . try $ pField "Production" pString
    quantity <- optionMaybe . try $ pField "Quantity" pQuantity
    temperature <- optionMaybe . try $ pField "Temperature" pTemperature
    time <- optionMaybe . try $ pField "Time" pTime
    cafeine <- optionMaybe . try $ pField "Cafeine" pPercentage
    skipMany eol

    let aTea = T.Tea name (Left fam) production quantity temperature time cafeine

    return aTea

parseFamilies :: String -> Either ParseError [F.Family]
parseFamilies = parse pFamiliesFile ""

pFamiliesFile :: Parser [F.Family]
pFamiliesFile = many pFamily <* eof

pFamily :: Parser F.Family
pFamily = do
    name <- pField "Name" pString
    icon <- pField "Icon" pString
    quantity <- pField "Quantity" pQuantity
    temperature <- pField "Temperature" pTemperature
    time <- pField "Time" pTime
    cafeine <- optionMaybe $ pField "Cafeine" pPercentage
    skipMany eol

    let aFamily = F.Family name icon quantity temperature time cafeine

    return aFamily

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

ignore :: Parser ()
ignore = pure ()

eol :: Parser ()
eol = pComment <||> (pSpaces <* newline)

pSpaces :: Parser ()
pSpaces = many (oneOf " \t") *> ignore

pComment :: Parser ()
pComment = char '#' *> manyTill anyChar (try newline) *> ignore

pField :: String -> Parser a -> Parser a
pField s r = do
        skipMany eol
        pSpaces *> string s *> pSpaces *> string ":" *> pSpaces *> r <* pSpaces <* eol

pString :: Parser [Char]
pString = id <$> many1 (noneOf "\n#")

pInt :: Parser Int
pInt = read <$> many1 digit

pDouble :: Parser Double
pDouble = do
    int <- many digit
    prec <- optionMaybe (char '.' *> digit <* many digit)
    let val = int ++ case prec of
                         Nothing -> []
                         Just v -> '.':[v]
    return (read val :: Double)

pQuantity :: Parser U.Quantity
pQuantity = choice [try pTspDl, try pTspOz]
    where pTspDl = do
                value <- pDouble <* pSpaces <* string "tsp/dl" <* optional (char '.')
                return . U.TspDl $ value
          pTspOz = do
                value <- pDouble <* pSpaces <* string "tsp/8oz" <* optional (char '.')
                return . U.TspOz $ value

pTemperature :: Parser U.Temperature
pTemperature = choice [try pCelsius, try pFahrenheit]
    where pCelsius = do
              value <- pInt <* pSpaces <* string "°C"
              return . U.Celsius $ value
          pFahrenheit = do
              value <- pInt <* pSpaces <* string "°F"
              return . U.Fahrenheit $ value

pTime :: Parser U.Time
pTime = do
    value <- pInt <* pSpaces <* string "s" <* optional (char '.')
    return . U.Second $ value

pPercentage :: Parser U.Percentage
pPercentage = do
    value <- pInt <* pSpaces <* char '%'
    return . U.Percent $ value


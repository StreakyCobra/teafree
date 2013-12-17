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


import Control.Applicative ((<*), (*>), (<$>), pure)
import Text.Parsec
import Text.Parsec.Text

import qualified Teafree.Entity.Family as Fam
import qualified Teafree.Entity.Tea as Tea
import qualified Teafree.Entity.Units as U

import Data.Text as T
default (T.Text)


parseTeas :: Text -> Either ParseError [Tea.Tea]
parseTeas = parse pTeasFile ""

pTeasFile :: Parser [Tea.Tea]
pTeasFile = many pTea <* eof

pTea :: Parser Tea.Tea
pTea = do
    name <- pField "Name" pText
    fam <- pField "Family" pText
    production <- optionMaybe . try $ pField "Production" pText
    quantity <- optionMaybe . try $ pField "Quantity" pQuantity
    temperature <- optionMaybe . try $ pField "Temperature" pTemperature
    time <- optionMaybe . try $ pField "Time" pTime
    cafeine <- optionMaybe . try $ pField "Cafeine" pPercentage
    note <- optionMaybe . try $ pNote
    skipMany eol

    let aTea = Tea.Tea name (Left fam) production quantity temperature time cafeine note

    return aTea

parseFamilies :: Text -> Either ParseError [Fam.Family]
parseFamilies = parse pFamiliesFile ""

pFamiliesFile :: Parser [Fam.Family]
pFamiliesFile = many pFamily <* eof

pFamily :: Parser Fam.Family
pFamily = do
    name <- pField "Name" pText
    icon <- pField "Icon" pText
    quantity <- pField "Quantity" pQuantity
    temperature <- pField "Temperature" pTemperature
    time <- pField "Time" pTime
    cafeine <- optionMaybe $ pField "Cafeine" pPercentage
    skipMany eol

    let aFamily = Fam.Family name icon quantity temperature time cafeine

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

pNote :: Parser [Text]
pNote = T.lines . T.pack <$> (sep *> manyTill anyChar (try sep))
    where sep = string "---\n"

pText :: Parser Text
pText = T.strip . T.pack <$> many1 (noneOf "\n#")

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


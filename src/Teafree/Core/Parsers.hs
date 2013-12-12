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

{-# LANGUAGE TemplateHaskell #-}

module Teafree.Core.Parsers where


import Control.Applicative ((<*), (*>), (<$>), pure)
import Data.Label
import Text.Parsec
import Text.Parsec.Text

import qualified Teafree.Entity.Family as Fam
import qualified Teafree.Entity.Tea as Tea
import qualified Teafree.Entity.Units as U

import Data.Text as T
default (T.Text)

fclabels [d|
    data CConfig = CConfig
        { cQuantity     :: U.Quantity -> U.Quantity
        , cTemperature  :: U.Temperature -> U.Temperature
        }
    |]

parseConfig :: Text -> Either ParseError CConfig
parseConfig = parse pConfigFile ""

pConfigFile :: Parser CConfig
pConfigFile = pConfig <* eof

pConfig :: Parser CConfig
pConfig = do
    quantity <- pField "Quantity" pQuantityTo
    temperature <- pField "Temperature" pTemperatureTo
    skipMany eol

    let cConfig = CConfig quantity temperature

    return cConfig

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
    cafeine <- optionMaybe . try $ pField "Cafeine" pPercentage
    skipMany eol

    let aFamily = Fam.Family name icon quantity temperature time cafeine

    return aFamily

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

pQuantityTo :: Parser (U.Quantity -> U.Quantity)
pQuantityTo = (string "2dl" >> pure U.toDl) <||> (string "8oz" >> pure U.toOz)

pTemperatureTo :: Parser (U.Temperature -> U.Temperature)
pTemperatureTo = (string "C" >> pure U.toC) <||> (string "F" >> pure U.toF)

pQuantity :: Parser U.Quantity
pQuantity = choice [try pTspDl, try pTspOz]
    where pTspDl = do
                value <- pDouble <* pSpaces <* string "tsp/2dl" <* optional (char '.')
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
    h <- optionMaybe . try $ pHour
    pSpaces
    m <- optionMaybe . try $ pMinute
    pSpaces
    s <- optionMaybe . try $ pSecond
    let vh = case h of
                 Nothing -> 0
                 Just v -> v
    let mh = case m of
                 Nothing -> 0
                 Just v -> v
    let sh = case s of
                 Nothing -> 0
                 Just v -> v
    return . U.Second $ vh + mh + sh

pHour :: Parser Int
pHour = do
    value <- pInt <* pSpaces <* string "h" <* optional (char '.')
    return . (*3600) $ value

pMinute :: Parser Int
pMinute = do
    value <- pInt <* pSpaces <* string "m" <* optional (char '.')
    return . (*60) $ value

pSecond :: Parser Int
pSecond = do
    value <- pInt <* pSpaces <* string "s" <* optional (char '.')
    return . (*1) $ value

pPercentage :: Parser U.Percentage
pPercentage = do
    value <- pInt <* pSpaces <* char '%'
    return . U.Percent $ value


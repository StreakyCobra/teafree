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

{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Teafree.Entity.Tea where


import Data.Label
import Prelude as P
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen as PP

import Teafree.Interaction.PPrint
import Teafree.Entity.Units
import qualified Teafree.Entity.Family as F

import Data.Text as T
default (T.Text)


fclabels [d|
    data Tea = Tea
        { name         :: Text
        , fam          :: Either Text F.Family
        , production   :: Maybe Text
        , _quantity    :: Maybe Quantity
        , _temperature :: Maybe Temperature
        , _time        :: Maybe Time
        , _cafeine     :: Maybe Percentage
        , note         :: Maybe [Text]
        }
    |]

instance Show Tea where
    show = show . pprint False

instance PPrint Tea where
    ppName c v = i (bold . dullred) $ text . T.unpack . get name $ v
            where i f = if c then f else id

    ppDetails c t = text (printf "%-15s" "Family:") <+> case get fam t of
                                                            Left _ -> (i (bold . dullred)) . text $ "/!\\ Wrong family"
                                                            Right v -> (i (bold . dullgreen) . text . T.unpack . get F.name $ v)
                    <$>
                    case get production t of
                        Nothing -> PP.empty
                        Just v -> text (printf "%-15s" "Production:") <+> (i (bold . dullblue) . text . T.unpack $ v) <$> PP.empty
                    <>
                    text (printf "%-15s" "Quantity:") <+> (pprint c $ quantity t) <$>
                    text (printf "%-15s" "Temperature:") <+> (pprint c $ temperature t) <$>
                    text (printf "%-15s" "Time:") <+> (pprint c $ time t) <$>
                    text (printf "%-15s" "Cafeine:") <+>
                    case (cafeine t) of
                        Just v -> pprint c v <+> text "of coffee"
                        Nothing -> i yellow $ text "Unknown"
                    <> case get note t of
                            Just v -> PP.empty <$> PP.empty <$> P.foldr1 (<$>) (P.map (text . T.unpack) v)
                            Nothing -> PP.empty
            where i f = if c then f else id

    ppSummary c v = text (T.unpack $ get name v) <+> text " (" <>
                    (pprint c $ quantity v) <> text " | " <>
                    (pprint c $ temperature v) <> text " | " <>
                    (pprint c $ time v) <>
                    (case (cafeine v) of
                        Just t -> text " | " <> pprint c t <+> text "of coffee"
                        Nothing -> text "") <>
                    text ")"

quantity :: Tea -> Quantity
quantity t = case (get _quantity t) of
                 Nothing -> case (get fam t) of
                                Left _ -> error "Wrong family"
                                Right f -> get F.quantity f
                 Just v -> v

temperature :: Tea -> Temperature
temperature t = case (get _temperature t) of
                    Nothing -> case (get fam t) of
                                   Left _ -> error "Wrong family"
                                   Right f -> get F.temperature f
                    Just v -> v

time :: Tea -> Time
time t = case (get _time t) of
             Nothing -> case (get fam t) of
                            Left _ -> error "Wrong family"
                            Right f -> get F.time f
             Just v -> v

cafeine :: Tea -> Maybe Percentage
cafeine t = case (get _cafeine t) of
                    Nothing -> case (get fam t) of
                                   Left _ -> error "Wrong family"
                                   Right f -> get F.cafeine f
                    v@(Just _) -> v

icon :: Tea -> Text
icon t = case (get fam t) of
             Left _ -> error "Wrong family"
             Right f -> get F.icon f

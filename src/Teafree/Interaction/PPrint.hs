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

module Teafree.Interaction.PPrint
    ( Colorized
    , PPrint(..)
    ) where


import Text.PrettyPrint.ANSI.Leijen


type Colorized = Bool

undef :: String
undef = "Not defined"

class PPrint a where
    ppName :: Colorized -> a -> Doc
    ppDetails :: Colorized -> a -> Doc
    ppSummary :: Colorized -> a -> Doc

    pprint :: Colorized -> a -> Doc
    pprint c a = (ppName c a) <$> indent 4 (ppDetails c a)

instance (PPrint a) => PPrint (Maybe a) where
    ppName True Nothing = yellow . text $ undef
    ppName False Nothing = text undef
    ppName c (Just v) = ppDetails c v

    ppDetails True Nothing = yellow . text $ undef
    ppDetails False Nothing = text undef
    ppDetails c (Just v) = ppDetails c v

    ppSummary True Nothing = yellow . text $ undef
    ppSummary False Nothing = text undef
    ppSummary c (Just v) = ppSummary c v


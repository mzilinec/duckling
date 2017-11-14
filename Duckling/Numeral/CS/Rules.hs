-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         v <- parseInt match
         integer $ toInteger v
      _ -> Nothing
  }

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nula", 0 )
  , ( "jeden", 1 )
  , ( "jedna", 1 )
  , ( "jedno", 1 )
  , ( "dva", 2 )
  , ( "dv\x0115", 2 )
  , ( "t\x0159i", 3 )
  , ( "čty\x0159i", 4 )
  , ( "p\x0115t", 5)
  , ( "šest", 6)
  , ( "sedm", 7)
  , ( "osm", 8)
  , ( "dev\x0115t", 9)
  , ( "deset", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nula|jed(en|n[ao])|dv(a|\x0115)|t(\x0159)i|(č)ty(\x0159)i|p(\x0115)t|(š)est|sedm|osm|dev(\x0115)t|deset)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneToNineteenMap >>= integer
      _ -> Nothing
  }

ruleTens :: Rule
ruleTens = Rule
  { name = "number (20..90)"
  , pattern = [ regex "(dvacet|t(\x0159)icet|(č)ty(\x0159)icet|pades(á)t|(š)edes(á)t|sedmdes(á)t|osmdes(á)t|devades(á)t)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of ten"
  , pattern =
    [ regex "(sto|dvěstě|třista|čtyřista|pětset|šestset|sedmset|osmset|devětset)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do {
        hundreds <- HashMap.lookup (Text.toLower match) hundredsMap >>= integer >>= withGrain 2 >>= withMultipliable;
        return hundreds
      }
      _ -> Nothing
  }

ruleCompositeTensWithSpace :: Rule
ruleCompositeTensWithSpace = Rule
  { name = "number 21..99 with space"
  , pattern =
    [ oneOf [20, 30..90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = tens}):
       Token Numeral (NumeralData {TNumeral.value = ones}):
       _) -> double $ tens + ones
      _ -> Nothing
  }

patternTens = "(dvacet|t\x0159icet|čty\x0159icet|padesát|šedesát|sedmdesát|osmdesát|devadesát)"
patternDigits = "(jed(en|n[ao])|dv(a|\x0115)|t\x0159i|čty\x0159i|p\x0115t|šest|sedm|osm|dev\x0115t)"

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "number 21..99"
  , pattern =
    [ regex $ ( patternDigits ++ patternTens ) ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (tens:ones:_)):_) -> (
              case (HashMap.lookup (Text.toLower tens) tensMap) of
                  Just n      -> integer $ ( n * 10 + fromMaybe 0 (HashMap.lookup (Text.toLower ones) oneToNineteenMap) )
                  Nothing     -> Nothing
             )
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleNumeral
  ]

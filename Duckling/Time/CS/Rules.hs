-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.CS.Rules
  ( rules ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG


ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"    , TG.Second, 0  , "hned"                             )
  , ("today"        , TG.Day   , 0  , "dnes(ka)?|dnešní den|dnešek"      )
  , ("tomorrow"     , TG.Day   , 1  , "zítra|zítrejšek|zítřejší den"     )
  , ("yesterday"    , TG.Day   , - 1, "včera"                            )
  , ("end of month" , TG.Month , 1  , "kon(ec|ci|cem) měsíce"            )
  , ("end of year"  , TG.Year  , 1  , "kon(ec|ci|cem) rok[au]"           )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "pondělí"     , "pondělí|pon?\\.?"         )
  , ( "úterý"       , "úterý|úte?\\.?"           )
  , ( "středa"      , "středa|stř?\\.?"          )
  , ( "čtvrtek"     , "čtvrtek|čtv?\\.?"         )
  , ( "pátek"       , "pátek|pát?\\.?"           )
  , ( "sobota"      , "sobota|sob?\\.?"          )
  , ( "neděle"      , "neděle|ned?\\.?"          )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Leden"     , "led(en|na|nu)|led\\.?"         )
  , ( "Únor"      , "únor(a|u)?|úno?\\.?"           )
  , ( "Březen"    , "břez(en|na|nu)|bře\\.?"        )
  , ( "Duben"     , "dub(en|na|nu)|dub\\.?"         )
  , ( "Květen"    , "květ(en|na|nu)|kvě\\.?"        )
  , ( "Červen"    , "červ(en|na|nu)"                )
  , ( "Červenec"  , "červen(ec|ce|ci)"              )
  , ( "Srpen"     , "srp(en|na|nu)|srp\\.?"         )
  , ( "Září"      , "září|zář?\\.?"                 )
  , ( "Říjen"     , "říj(en|na|nu)|říj\\.?"         )
  , ( "Listopad"  , "listopadu?|lis\\.?"            )
  , ( "Prosinec"  , "prosin(ec|ce|ci)|pro\\.?"      )
  ]

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "teď"
    ]
  , prod = \_ -> tt now
  }

rules :: [Rule]
rules =
  [
--    ruleIntersect
--  , ruleIntersectOf
--  , ruleAbsorbOnTime
--  , ruleAbsorbOnADOW
--  , ruleAbsorbInMonth
--  , ruleAbsorbCommaTOD
--  , ruleNextDOW
--  , ruleNextTime
--  , ruleThisTime
--  , ruleLastTime
--  , ruleTimeBeforeLastAfterNext
--  , ruleLastDOWOfTime
--  , ruleLastCycleOfTime
--  , ruleLastWeekendOfMonth
--  , ruleNthTimeOfTime
--  , ruleTheNthTimeOfTime
--  , ruleNthTimeAfterTime
--  , ruleTheNthTimeAfterTime
--  , ruleYear
--  , ruleYearPastLatent
--  , ruleYearFutureLatent
--  , ruleTheDOMNumeral
--  , ruleTheDOMOrdinal
--  , ruleDOMLatent
--  , ruleNamedDOMOrdinal
--  , ruleMonthDOMNumeral
--  , ruleDOMMonth
--  , ruleDOMOfMonth
--  , ruleDOMOrdinalMonthYear
--  , ruleIdesOfMonth
--  , ruleTODLatent
--  , ruleAtTOD
--  , ruleTODOClock
--  , ruleHHMM
--  , ruleHHMMLatent
--  , ruleHHMMSS
--  , ruleMilitaryAMPM
--  , ruleTODAMPM
--  , ruleHONumeral
--  , ruleHODHalf
--  , ruleHODQuarter
--  , ruleNumeralToHOD
--  , ruleHalfToHOD
--  , ruleQuarterToHOD
--  , ruleNumeralAfterHOD
--  , ruleHalfAfterHOD
--  , ruleQuarterAfterHOD
--  , ruleHalfHOD
--  , ruleYYYYMMDD
--  , ruleMMYYYY
--  , ruleNoonMidnightEOD
--  , rulePartOfDays
--  , ruleEarlyMorning
--  , rulePODIn
--  , rulePODThis
--  , ruleTonight
--  , ruleAfterPartofday
--  , ruleTimePOD
--  , rulePODofTime
--  , ruleWeekend
--  , ruleSeasons
--  , ruleTODPrecision
--  , rulePrecisionTOD
--  , ruleIntervalFromMonthDDDD
--  , ruleIntervalFromDDDDMonth
--  , ruleIntervalMonthDDDD
--  , ruleIntervalDDDDMonth
--  , ruleIntervalDash
--  , ruleIntervalFrom
--  , ruleIntervalBetween
--  , ruleIntervalTODDash
--  , ruleIntervalTODFrom
--  , ruleIntervalTODAMPM
--  , ruleIntervalTODBetween
--  , ruleIntervalBy
--  , ruleIntervalByTheEndOf
--  , ruleIntervalUntilTOD
--  , ruleIntervalAfterTOD
--  , ruleIntervalSinceTOD
--  , ruleMemorialDay
--  , ruleMemorialDayWeekend
--  , ruleLaborDayWeekend
--  , ruleCycleThisLastNext
--  , ruleCycleTheAfterBeforeTime
--  , ruleCycleAfterBeforeTime
--  , ruleCycleLastNextN
--  , ruleCycleOrdinalOfTime
--  , ruleCycleTheOrdinalOfTime
--  , ruleCycleTheOfTime
--  , ruleCycleOrdinalAfterTime
--  , ruleCycleTheOrdinalAfterTime
--  , ruleCycleOrdinalQuarter
--  , ruleCycleTheOrdinalQuarter
--  , ruleCycleOrdinalQuarterYear
--  , ruleDurationInWithinAfter
--  , ruleDurationHenceAgo
--  , ruleDurationAfterBeforeTime
--  , ruleIntervalForDurationFrom
--  , ruleInNumeral
--  , ruleTimezone
--  , rulePartOfMonth
  ruleNow
--  , ruleBlackFriday
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
--  ++ ruleUSHolidays
--  ++ ruleMoreUSHolidays

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
  , ( "středa"      , "střed[ay]|stř?\\.?"          )
  , ( "čtvrtek"     , "čtvrt(?:ek|ku|ka)|čtv?\\.?"         )
  , ( "pátek"       , "pát(?:ek|ku)|pát?\\.?"           )
  , ( "sobota"      , "sobot[ayu]|sob?\\.?"          )
  , ( "neděle"      , "neděl[ei]|ned?\\.?"          )
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

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "ráno|dopoledne"
    ]
  , prod = \_ ->
      let from = hour False 6
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "noon"
  , pattern =
    [ regex "(na )?oběd?|(ve )?dvanáct( hodin)?"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "odpoledne"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "večer"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "noc|v noci"
    ]
  , prod = \_ ->
      let from = hour False 22
          to = hour False 4
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleThisPartofday :: Rule
ruleThisPartofday = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "dnešní|dnes"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay <$>
        intersect (cycleNth TG.Day 0) td
      _ -> Nothing
  }

ruleInduringThePartofday :: Rule
ruleInduringThePartofday = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(in|an|am|w(ä)h?rend)( der| dem| des)?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleOnANamedday :: Rule
ruleOnANamedday = Rule
  { name = "on a named-day"
  , pattern =
    [ regex "v"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleFromDatetimeDatetimeInterval :: Rule
ruleFromDatetimeDatetimeInterval = Rule
  { name = "from <datetime> - <datetime> (interval)"
  , pattern =
    [ regex "od"
    , dimension Time
    , regex "\\-|do"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleDatetimeDatetimeInterval :: Rule
ruleDatetimeDatetimeInterval = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|do"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.h]([0-5]\\d)(?:h|hod|m|min)?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleDDMM :: Rule
ruleDDMM = Rule
  { name = "dd.mm"
  , pattern =
    [ regex "(?:am\\s+)?([012]?[1-9]|10|20|30|31)\\. ?(10|11|12|0?[1-9])\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
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
  , ruleMorning
  , ruleNoon
  , ruleAfternoon
  , ruleEvening
  , ruleNight
  , ruleOnANamedday
  , ruleTimePartofday
  , ruleThisPartofday
  , ruleInduringThePartofday
  , ruleFromDatetimeDatetimeInterval
  , ruleDatetimeDatetimeInterval
  , ruleHhmm
  , ruleDDMM
--  , ruleBlackFriday
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
--  ++ ruleUSHolidays
--  ++ ruleMoreUSHolidays

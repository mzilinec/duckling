-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.CS.Corpus
  ( corpus
--  , defaultCorpus
--  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext {locale = makeLocale CS Nothing}, allExamples)


allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "hned"
             , "teď"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "dnes"
             , "dneska"
             ]
--  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
--             [ "2/2013"
--             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "včera"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "zítra"
             , "zítřejší den"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "koncem roka"
             ,  "konec roku"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "čtvrtek"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "březen"
             ]
  ]

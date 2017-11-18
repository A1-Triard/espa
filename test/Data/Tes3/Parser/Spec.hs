--
-- Copyright 2016, 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Data.Tes3.Parser.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3
import Data.Tes3.Parser

tests :: Test
tests = TestList
  [ TestCase parseValidFile
  , TestCase parseInvalidFile
  ]

testFile1Text :: S.Text
testFile1Text
  =  "TES3\n"
  <> "HEDR ESP 1067869798\n"
  <> "    Ath\n"
  <> "    Descr descr descr.\n"
  <> "MAST Morrowind.esm\n"
  <> "DATA 79764287\n"
  <> "\n"
  <> "CLOT\n"
  <> "NAME _ale_leather_skirt\n"
  <> "MODL Aleanne\\\\dr_a_fC_la_25_gnd.nif\n"
  <> "FNAM Длинная тога\n"
  <> "CTDT AgAAAAAAQEB4AFgC\n"
  <> "CNAM _ale_dr_a_fC_1_025%\n"
  <> "\n"
  <> "CLOT Blocked\n"
  <> "NAME _ale_short_toga\n"
  <> "MODL Aleanne\\\\dr_b_fC_la_20s_gnd.nif\n"
  <> "FNAM Короткая тога\n"
  <> "CTDT AgAAAAAAgD94AFgC\n"
  <> "CNAM _ale_dr_b_fC_la_20s%\n"

invalidTestFileText :: S.Text
invalidTestFileText
  =  "TES3\n"
  <> "HEDR ESP 1067869798\n"
  <> "    Ath\n"
  <> "   Descr descr descr.\n"
  <> "MAST Morrowind.esm\n"
  <> "DATA 79764287\n"
  <> "\n"
  <> "CLOT\n"
  <> "NAME _ale_leather_skirt\n"
  <> "MODL Aleanne\\\\dr_a_fC_la_25_gnd.nif\n"
  <> "FNAM Длинная тога\n"
  <> "CTDT AgAAAAAAQEB4AFgC\n"
  <> "CNAM _ale_dr_a_fC_1_025%\n"
  <> "\n"
  <> "CLOT Blocked\n"
  <> "NAME _ale_short_toga\n"
  <> "MODL Aleanne\\\\dr_b_fC_la_20s_gnd.nif\n"
  <> "FNAM Короткая тога\n"
  <> "CTDT AgAAAAAAgD94AFgC\n"
  <> "CNAM _ale_dr_b_fC_la_20s%\n"

testFile1 :: [T3Record]
testFile1 =
  [ T3Record (T3Mark TES3) t3FlagsEmpty
    [ T3HeaderField (T3Mark HEDR) (T3FileHeader 1067869798 ESP "Ath" ["Descr descr descr."])
    , T3StringField (T3Mark MAST) "Morrowind.esm\0"
    , T3LongField (T3Mark DATA) 79764287
    ]
  , T3Record (T3Mark CLOT) t3FlagsEmpty
    [ T3StringField (T3Mark NAME) "_ale_leather_skirt\0"
    , T3StringField (T3Mark MODL) "Aleanne\\dr_a_fC_la_25_gnd.nif\0"
    , T3StringField (T3Mark FNAM) "Длинная тога\0"
    , T3BinaryField (T3Mark CTDT) "\STX\NUL\NUL\NUL\NUL\NUL@@x\NULX\STX"
    , T3StringField (T3Mark CNAM) "_ale_dr_a_fC_1_025"
    ]
  , T3Record (T3Mark CLOT) (t3FlagsEmpty { t3Blocked = True })
    [ T3StringField (T3Mark NAME) "_ale_short_toga\0"
    , T3StringField (T3Mark MODL) "Aleanne\\dr_b_fC_la_20s_gnd.nif\0"
    , T3StringField (T3Mark FNAM) "Короткая тога\0"
    , T3BinaryField (T3Mark CTDT) "\STX\NUL\NUL\NUL\NUL\NUL\128?x\NULX\STX"
    , T3StringField (T3Mark CNAM) "_ale_dr_b_fC_la_20s"
    ]
  ]

pT3File :: T.Parser [T3Record]
pT3File = do
  header <- pT3Record
  records <- many (Tp.endOfLine *> pT3Record)
  return (header : records)

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (Right testFile1) $ TP.parseOnly (pT3File <* Tp.endOfInput) testFile1Text

parseInvalidFile :: Assertion
parseInvalidFile = do
  assertEqual "" (Left "endOfInput") $ TP.parseOnly (pT3File <* Tp.endOfInput) invalidTestFileText

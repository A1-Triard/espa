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

module Data.Tes3.Utils.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3.Utils

tests :: Test
tests = TestList
  [ TestCase writeRunTest
  , TestCase pRunTest
  , TestCase writeLinesTest
  , TestCase pLinesTest
  , TestCase writeNamesTest
  , TestCase pNamesTest
  ]

writeRunTest :: Assertion
writeRunTest = do
  assertEqual "" "Qa\\ Bc\\ De" $ writeRun "Qa Bc De"
  assertEqual "" "Qa\\ Bc\\ De\\ " $ writeRun "Qa Bc De "
  assertEqual "" "Qa\\ Bc\\ \\rDe\\ " $ writeRun "Qa Bc \rDe "
  assertEqual "" "\\x07\\x07\\x07\\\\\\ " $ writeRun "\7\7\7\\ "

pRunTest :: Assertion
pRunTest = do
  assertEqual "" (Right "Qa Bc De") $ TP.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ De"
  assertEqual "" (Right "Qa Bc De ") $ TP.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ De\\ "
  assertEqual "" (Right "Qa Bc \rDe ") $ TP.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ \\rDe\\ "
  assertEqual "" (Right "\7\7\7\\ ") $ TP.parseOnly (pRun <* Tp.endOfInput) "\\x07\\x07\\x07\\\\\\ "

writeLinesTest :: Assertion
writeLinesTest = do
  assertEqual "" "    Qa Bc\n     De\n    \n    \n" $ writeLines ["Qa Bc", " De", "", ""]

pLinesTest :: Assertion
pLinesTest = do
  assertEqual "" (Right ["Qa Bc", " De", "", ""]) $ TP.parseOnly (pLines <* Tp.endOfInput) "    Qa Bc\r\n     De\n    \n    \n"

writeNamesTest :: Assertion
writeNamesTest = do
  assertEqual "" "abcd;Defg;;\n" $ writeNames ["abcd", "Defg", ""]

pNamesTest :: Assertion
pNamesTest = do
  assertEqual "" (Right ["abcd", "Defg", ""]) $ TP.parseOnly (pNames <* Tp.endOfInput) "abcd;Defg;;\n"

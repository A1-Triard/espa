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
  assertEqual "" "Qa\\ Bc\\ De" $ runConduitPure $ runTextGen (genRun "Qa Bc De") .| N.sinkLazy
  assertEqual "" "Qa\\ Bc\\ De\\ " $ runConduitPure $ runTextGen (genRun "Qa Bc De ") .| N.sinkLazy
  assertEqual "" "Qa\\ Bc\\ \\rDe\\ " $ runConduitPure $ runTextGen (genRun "Qa Bc \rDe ") .| N.sinkLazy
  assertEqual "" "\\x07\\x07\\x07\\\\\\ " $ runConduitPure $ runTextGen (genRun "\7\7\7\\ ") .| N.sinkLazy

pRunTest :: Assertion
pRunTest = do
  assertEqual "" ((Right "Qa Bc De") :: Either Void Text) $ runConduitPure $ yield "Qa\\ Bc\\ De" .| runParser pRun
  assertEqual "" ((Right "Qa Bc De ") :: Either Void Text) $ runConduitPure $ yield "Qa\\ Bc\\ De\\ " .| runParser pRun
  assertEqual "" ((Right "Qa Bc \rDe ") :: Either Void Text) $ runConduitPure $ yield "Qa\\ Bc\\ \\rDe\\ " .| runParser pRun
  assertEqual "" ((Right "\7\7\7\\ ") :: Either Void Text) $ runConduitPure $ yield "\\x07\\x07\\x07\\\\\\ " .| runParser pRun

writeLinesTest :: Assertion
writeLinesTest = do
  assertEqual "" "    Qa Bc\n     De\n    \n    \n" $ runConduitPure $ runTextGen (genLines ["Qa Bc", " De", "", ""]) .| N.sinkLazy

pLinesTest :: Assertion
pLinesTest = do
  assertEqual "" ((Right ["Qa Bc", " De", "", ""]) :: Either Void [Text]) $ runConduitPure $ yield "    Qa Bc\r\n     De\n    \n    \n" .| runParser pLines

writeNamesTest :: Assertion
writeNamesTest = do
  assertEqual "" "abcd;Defg;;\n" $ runConduitPure $ runTextGen (genNames ["abcd", "Defg", ""]) .| N.sinkLazy

pNamesTest :: Assertion
pNamesTest = do
  assertEqual "" (Right ["abcd", "Defg", ""]) $ runConduitPure $ yield "abcd;Defg;;\n" .| runParser pNames

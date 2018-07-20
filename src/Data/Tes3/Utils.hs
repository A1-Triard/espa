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

module Data.Tes3.Utils
  ( genNulledLine
  , pNulledLine
  , genLine
  , pLine
  , genNulledRun
  , pNulledRun
  , genRun
  , pRun
  , genLines
  , pLines
  , genNames
  , pNames
  , pT3Sign
  ) where

#include <haskell>
import Data.Tes3

genEscapedChar :: Bool -> Bool -> Bool -> Char -> TextGen
genEscapedChar escape_semicolon escape_percent escape_spaces c
  | c == '\0' = "\\0"
  | c == '\\' = "\\\\"
  | c == '\r' = "\\r"
  | c == '\n' = "\\n"
  | escape_semicolon && c == ';' = "\\;"
  | escape_percent && c == '%' = "\\%"
  | escape_spaces && c == ' ' = "\\ "
  | escape_spaces && c == '\t' = "\\t"
  | c == '\t' = "\t"
  | ord c < 32 || ord c == 127 = "\\x" <> genHexByte True (fromIntegral $ ord c)
  | otherwise = genString $ ST.singleton c

pEscapedChar :: Bool -> Bool -> Bool -> Parser () Char
pEscapedChar allow_semicolon allow_percent allow_spaces = do
  c <- pChar
  case c of
    '\r' -> throwError ()
    '\n' -> throwError ()
    '\\' -> special
    '\t' -> if allow_spaces then return '\t' else throwError ()
    ' ' -> if allow_spaces then return ' ' else throwError ()
    '%' -> if allow_percent then return '%' else throwError ()
    ';' -> if allow_semicolon then return ';' else throwError ()
    s -> return s
  where
  special :: Parser () Char
  special = do
    c <- pChar
    case c of
      '0' -> return '\0'
      '%' -> return '%'
      ';' -> return ';'
      'r' -> return '\r'
      'n' -> return '\n'
      't' -> return '\t'
      '\\' -> return '\\'
      ' ' -> return ' '
      'x' -> (chr . fromIntegral) <$> pHexByte
      _ -> throwError ()

genEscapedText :: Bool -> Bool -> Bool -> Text -> TextGen
genEscapedText escape_semicolon escape_percent escape_spaces =
  mapM_ (genEscapedChar escape_semicolon escape_percent escape_spaces) . T.unpack

pEscapedText :: Bool -> Bool -> Bool -> Parser e Text
pEscapedText allow_semicolon allow_percent allow_spaces =
  T.pack <$> many'' (pEscapedChar allow_semicolon allow_percent allow_spaces)

genNulledText :: Bool -> Text -> TextGen
genNulledText escape_spaces (T.stripSuffix "\0" -> Just s) = genEscapedText False True escape_spaces s
genNulledText escape_spaces s = genEscapedText False True escape_spaces s >> "%"

genNulledLine :: Text -> TextGen
genNulledLine s = genNulledText False s >> "\n"

genNulledRun :: Text -> TextGen
genNulledRun = genNulledText True

pNulledText :: Bool -> Parser e Text
pNulledText allow_spaces = do
  s <- pEscapedText True False allow_spaces
  add_null <- maybe True (const False) <$> option'' (pCharIs '%')
  if add_null
    then return $ s <> "\0"
    else return s

pNulledLine :: Parser () Text
pNulledLine = do
  s <- pNulledText True
  skipEndOfLine
  return s

pNulledRun :: Parser e Text
pNulledRun = pNulledText False

genText :: Bool -> Text -> TextGen
genText = genEscapedText False False

genLine :: Text -> TextGen
genLine s = genText False s >> "\n"

genRun :: Text -> TextGen
genRun = genText True

pText :: Bool -> Parser e Text
pText = pEscapedText True True

pLine :: Parser () Text
pLine = do
  s <- pText True
  skipEndOfLine
  return s

pRun :: Parser e Text
pRun = pText False

genLines :: [Text] -> TextGen
genLines = mapM_ (\ !x -> "    " <> genEscapedText False False False x <> "\n")

pLines :: Parser e [Text]
pLines = many'' $ do
  skipStringIs "    "
  s <- pEscapedText True True True
  skipEndOfLine
  return s

genNames :: [Text] -> TextGen
genNames names = (forM_ names $ \ !x -> genEscapedText True False False x <> ";") >> "\n"

pNames :: Parser () [Text]
pNames = do
  s <- p
  skipEndOfLine
  return s
  where
  p = many'' $ do
    s <- pEscapedText False True True
    skipCharIs ';'
    return s

pT3Sign :: Parser () T3Sign
pT3Sign = do
  a <- pChar
  b <- pChar
  c <- pChar
  d <- pChar
  return $ t3SignNew $ fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)

fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fromBytes a b c d
  =  (fromIntegral a)
  .|. (shift (fromIntegral b) 8)
  .|. (shift (fromIntegral c) 16)
  .|. (shift (fromIntegral d) 24)

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

module Data.Tes3.Assembler
  ( assembly
  ) where

#include <haskell>
import Data.Tes3
import Data.Tes3.Parser
import Data.Tes3.Put
import Data.Conduit.Parsers.GetC (runMaybeG, maybeG)

pMaybeT3Record :: Parser String (Maybe T3Record)
pMaybeT3Record = runMaybeG $ do
  !e <- N.nullE
  if e
    then maybeG $ return Nothing
    else return ()
  skipEndOfLine ?>> return "EOL expected"
  !c <- peekChar
  if c == Just '#'
    then maybeG $ return Nothing
    else pT3Record

pRecordsNumber :: Parser () Word32
pRecordsNumber = do
  skipCharIs '#'
  skipCharIs ' '
  n <- pDecimal
  skipEndOfLine
  return n

lineColumnError :: String -> Parser Void String
lineColumnError !m = do
  !l <- linesRead
  !c <- columnsRead
  !p <- peekChar
  return $ m ++ " at " ++ show (l + 1) ++ ":" ++ show (c + 1) ++ " (" ++ show p ++ ")."

assembly :: Monad m => ConduitT S.Text S.ByteString m (Either String (T3FileType, Word32))
assembly = runParser $ do
  T3Record rs rz rfields <- pT3Record ?=>> \e -> lineColumnError $ "First record error (" ++ e ++ ")."
  runPut $ putT3Record $ T3Record rs rz rfields
  if rs /= T3Mark TES3
    then throwError "Invalid file format."
    else return ()
  file_type <- case rfields of
    (T3HeaderField (T3Mark HEDR) (T3FileHeader _ t _ _) : _) -> return t
    _ -> throwError "Invalid file header."
  n <- (flip execStateT) 0 $ untilJust $ do
    !mr <- lift $ pMaybeT3Record ?=>> \e -> lineColumnError $ "A record error (" ++ e ++ ")."
    case mr of
      Nothing -> return $ Just ()
      Just !r -> do
        modify' (+ 1)
        lift $ runPut $ putT3Record r
        return Nothing
  items_count <- fromMaybe n <$> option'' pRecordsNumber
  if items_count < n
    then throwError $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed."
    else return ()
  endOfInput ?>> lineColumnError "EOF error"
  return (file_type, items_count)

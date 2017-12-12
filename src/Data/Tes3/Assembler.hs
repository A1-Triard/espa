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

conduit :: Monad m => T.Parser a -> ConduitM S.Text o (ExceptT String m) a
conduit parser = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      !inp <- maybe ST.empty toNullable <$> N.awaitNonNull
      go $ p inp
    go (TP.Done unused result) = do
      if ST.null unused
        then return ()
        else leftover unused
      return result
    go (TP.Fail unused _ err) = do
      if ST.null unused
        then return ()
        else leftover unused
      throwError err

pMaybeT3Record :: T.Parser (Maybe T3Record)
pMaybeT3Record = do
  Tp.endOfLine
  c <- Tp.peekChar
  if c == Just '#'
    then return Nothing
    else Just <$> pT3Record

pRecordsNumber :: T.Parser Word32
pRecordsNumber = do
  void $ Tp.char '#'
  void $ Tp.char ' '
  n <- Tp.decimal
  Tp.endOfLine
  return n

assembly :: Monad m => ConduitM S.Text S.ByteString m (Either String (T3FileType, Word32))
assembly = runExceptC $ do
  T3Record rs rz rfields <- conduit (pT3Record <?> "H")
  runPut $ putT3Record $ T3Record rs rz rfields
  if rs /= T3Mark TES3
    then throwError "Invalid file format."
    else return ()
  file_type <- case rfields of
    (T3HeaderField (T3Mark HEDR) (T3FileHeader _ t _ _) : _) -> return t
    _ -> throwError "Invalid file header."
  n <- (flip execStateT) 0 $ untilJust $ do
    end <- lift N.nullE
    if end
      then return $ Just ()
      else do
        !mr <- lift $ conduit (pMaybeT3Record <?> "R")
        case mr of
          Nothing -> return $ Just ()
          Just !r -> do
            modify' (+ 1)
            lift $ runPut $ putT3Record r
            return Nothing
  items_count <- conduit (Tp.option n pRecordsNumber <?> "RN")
  if items_count < n
    then throwError $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed."
    else return ()
  conduit (Tp.endOfInput <?> "EOF")
  return (file_type, items_count)

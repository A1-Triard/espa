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

conduitParser1 :: Monad m => T.Parser a -> ConduitM S.Text a m (Either String a)
conduitParser1 parser = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      !inp <- maybe ST.empty toNullable <$> N.awaitNonNull
      go $ p inp
    go (TP.Done unused result) = do
      yield result
      if ST.null unused
        then return ()
        else leftover unused
      return $ Right result
    go (TP.Fail _ _ err) = do
      return $ Left err

conduitParserN :: (Monad m, Num n) => T.Parser (Maybe a) -> n -> ConduitM S.Text a m (Either String (Maybe n))
conduitParserN parser n = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      !inp <- maybe ST.empty toNullable <$> N.awaitNonNull
      go $ p inp
    go (TP.Done unused result) = do
      case result of
        Nothing -> return ()
        Just r -> yield r
      if ST.null unused
        then return ()
        else leftover unused
      return $ Right $ if isJust result then Just (n + 1) else Nothing
    go (TP.Fail _ c err) = do
      return $ Left $ err ++ " (" ++ intercalate " -> " c ++ ")"

conduitRepeatE :: (Monad m, MonoFoldable seq) => a -> (a -> ConduitM seq r m (Either e (Maybe a))) -> ConduitM seq r m (Either e a)
conduitRepeatE a0 produce =
  go a0
  where
    go an = do
      end <- N.nullE
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right (Just an_1) -> go an_1
            Right Nothing -> return $ Right an

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

assembly :: Monad m => ConduitM S.Text ByteString m (Either String (T3FileType, Word32))
assembly = runExceptT $ do
  T3Record rs _ rfields <- (hoistEither =<<) $ lift $ mapOutput putT3Record $ conduitParser1 (pT3Record <?> "H")
  if rs /= T3Mark TES3
    then hoistEither $ Left $ "Invalid file format."
    else return ()
  file_type <- case rfields of
    (T3HeaderField (T3Mark HEDR) (T3FileHeader _ t _ _) : _) -> return t
    _ -> hoistEither $ Left $ "Invalid file header."
  n <- (hoistEither =<<) $ lift $ mapOutput putT3Record $ conduitRepeatE 0 $ conduitParserN (pMaybeT3Record <?> "R")
  items_count <- (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.option n pRecordsNumber <?> "RN")
  if items_count < n
    then hoistEither $ Left $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed."
    else return ()
  (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.endOfInput <?> "EOF")
  return (file_type, items_count)

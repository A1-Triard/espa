module Data.Tes3.Assembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Parser
import Data.Tes3.Put

awaitE :: Monad m => ConduitM S.Text a m S.Text
awaitE = do
  maybe_inp <- await
  case maybe_inp of
    Nothing -> return ST.empty
    Just (ST.null -> True) -> awaitE
    Just inp -> return inp

conduitParser1 :: Monad m => T.Parser a -> ConduitM S.Text a m (Either String a)
conduitParser1 parser = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      inp <- awaitE
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
      inp <- awaitE
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

conduitRepeat :: Monad m => a -> (a -> ConduitM seq r m (Either e (Maybe a))) -> ConduitM seq r m (Either e a)
conduitRepeat a0 produce =
  go a0
  where
    go an = do
      end <- N.null
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right (Just an_1) -> go an_1
            Right Nothing -> return $ Right an

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
  (hoistEither =<<) $ lift $ mapOutput (const putT3FileSignature) $ conduitParser1 (pT3FileSignature <?> "S")
  T3FileHeader _ file_type _ _ _ <- (hoistEither =<<) $ lift $ mapOutput putT3FileHeader $ conduitParser1 (pT3FileHeader <?> "H")
  n <- (hoistEither =<<) $ lift $ mapOutput putT3Record $ conduitRepeatE 0 $ conduitParserN (pMaybeT3Record <?> "R")
  items_count <- (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.option n pRecordsNumber <?> "RN")
  if items_count < n
    then hoistEither $ Left $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed."
    else return ()
  (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.endOfInput <?> "EOF")
  return (file_type, items_count)

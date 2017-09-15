module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Get
import Data.Tes3.Utils

writeT3Field :: T3Field -> Text
writeT3Field (T3BinaryField sign d) = T.pack (show sign) <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field (T3StringField sign s) = T.pack (show sign) <> " " <> writeNulledLine s
writeT3Field (T3MultilineField sign t) = T.pack (show sign) <> "\n" <> writeLines t
writeT3Field (T3MultiStringField sign t) = T.pack (show sign) <> " " <> writeNames t
writeT3Field (T3RefField sign z n) = T.pack (show sign) <> " " <> T.pack (show z) <> " " <> writeLine n
writeT3Field (T3FloatField sign v) = T.pack (show sign) <> " " <> T.pack (show $ float2Double v) <> "\n"
writeT3Field (T3IntField sign v) = T.pack (show sign) <> " " <> T.pack (show v) <> "\n"
writeT3Field (T3ShortField sign v) = T.pack (show sign) <> " " <> T.pack (show v) <> "\n"
writeT3Field (T3LongField sign v) = T.pack (show sign) <> " " <> T.pack (show v) <> "\n"
writeT3Field (T3ByteField sign v) = T.pack (show sign) <> " " <> T.pack (show v) <> "\n"
writeT3Field (T3CompressedField sign d) = T.pack (show sign) <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field
  ( T3IngredientField sign
    ( T3IngredientData weight value
      (T3IngredientEffects e1 e2 e3 e4)
      (T3IngredientSkills s1 s2 s3 s4)
      (T3IngredientAttributes a1 a2 a3 a4)
    )
  ) =
  T.pack (show sign) <> "\n"
    <> "    " <> T.pack (show weight) <> " " <> T.pack (show value) <> "\n"
    <> "    " <> T.pack (show e1) <> " " <> T.pack (show e2) <> " " <> T.pack (show e3) <> " " <> T.pack (show e4) <> "\n"
    <> "    " <> T.pack (show s1) <> " " <> T.pack (show s2) <> " " <> T.pack (show s3) <> " " <> T.pack (show s4) <> "\n"
    <> "    " <> T.pack (show a1) <> " " <> T.pack (show a2) <> " " <> T.pack (show a3) <> " " <> T.pack (show a4) <> "\n"
writeT3Field
  ( T3ScriptField sign
    ( T3ScriptHeader name
      shorts longs floats
      data_size var_table_size
    )
  ) =
  T.pack (show sign)
    <> " " <> writeRun name
    <> " " <> T.pack (show shorts) <> " " <> T.pack (show longs) <> " " <> T.pack (show floats)
    <> " " <> T.pack (show data_size) <> " " <> T.pack (show var_table_size)
    <> "\n"
writeT3Field (T3DialField sign t) = T.pack (show sign) <> either (\x -> if x == 0 then T.empty else " " <> T.pack (show x)) ((" " <> ) . writeEnum 2) t <> "\n"
writeT3Field (T3NoneField sign) = T.pack (show sign) <> "\n"
writeT3Field (T3HeaderField sign (T3FileHeader version file_type author description)) =
  T.pack (show sign) <> " " <> writeEnum 0 file_type <> " " <> T.pack (show version) <> " " <> writeLine author <> writeLines description

writeT3Flags :: T3Flags -> Text
writeT3Flags f =
  ""
  <> (if t3Deleted f then " Deleted" else "")
  <> (if t3Blocked f then " Blocked" else "")
  <> (if t3Persist f then " Persist" else "")

writeT3Record :: T3Record -> Text
writeT3Record (T3Record sign fl fields)
  =  T.pack (show sign) <> writeT3Flags fl <> "\n"
  <> T.concat [writeT3Field f | f <- fields]

conduitGet1 :: Monad m => Get e a -> ByteOffset -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, a))
conduitGet1 g base_offset = do
  go $ runGetIncremental base_offset g
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, result)
    go (G.Fail _ offset err) = do
      return $ Left (offset, err)

conduitGetN :: (Monad m, Num n) => Get e a -> (ByteOffset, n) -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, n))
conduitGetN g (base_offset, n) = do
  go $ runGetIncremental base_offset g
  where
    go (G.Partial p) = do
      inp <- await
      go $ p inp
    go (G.Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, n + 1)
    go (G.Fail _ offset err) = do
      return $ Left (offset, err)

conduitRepeat :: Monad m => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
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
            Right an_1 -> go an_1

conduitRepeatE :: (Monad m, MonoFoldable seq) => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
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
            Right an_1 -> go an_1

disassembly :: Monad m => Bool -> (T3Sign -> Bool) -> Word32 -> ConduitM S.ByteString Text m (Either (ByteOffset, Either String (ByteOffset -> String)) ())
disassembly adjust skip_record items_count = runExceptT $ do
  let write_rec first (T3Record s a b) = if skip_record s then T.empty else (if first then "" else "\n") <> writeT3Record (T3Record s a b)
  (r, _) <- (hoistEither =<<) $ lift $ mapOutput (write_rec True) $ conduitGet1 (getT3Record adjust) 0
  (f, n) <- (hoistEither =<<) $ lift $ mapOutput (write_rec False) $ conduitRepeatE (r, 0) $ conduitGetN $ getT3Record adjust
  if n > items_count
    then hoistEither $ Left (f, Right $ const $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed.")
    else return ()
  if n /= items_count
    then lift $ yield $ "\n" <> "# " <> T.pack (show items_count) <> "\n"
    else return ()

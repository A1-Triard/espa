module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Get
import Data.Tes3.Utils

write :: Show a => a -> Text
write = T.pack . show

writeNpcChar :: T3NpcDataChar -> Text
writeNpcChar d
  =  " " <> write (t3NpcStrength d) <> " " <> write (t3NpcIntelligence d) <> " " <> write (t3NpcWillpower d) <> " " <> write (t3NpcAgility d)
  <> " " <> write (t3NpcSpeed d) <> " " <> write (t3NpcEndurance d) <> " " <> write (t3NpcPersonality d) <> " " <> write (t3NpcLuck d)
  <> " " <> write (t3NpcBlock d) <> " " <> write (t3NpcArmorer d) <> " " <> write (t3NpcMediumArmor d) <> " " <> write (t3NpcHeavyArmor d)
  <> " " <> write (t3NpcBluntWeapon d) <> " " <> write (t3NpcLongBlade d) <> " " <> write (t3NpcAxe d) <> " " <> write (t3NpcSpear d)
  <> " " <> write (t3NpcAthletics d) <> " " <> write (t3NpcEnchant d) <> " " <> write (t3NpcDestruction d) <> " " <> write (t3NpcAlteration d)
  <> " " <> write (t3NpcIllusion d) <> " " <> write (t3NpcConjuration d) <> " " <> write (t3NpcMysticism d) <> " " <> write (t3NpcRestoration d)
  <> " " <> write (t3NpcAlchemy d) <> " " <> write (t3NpcUnarmored d) <> " " <> write (t3NpcSecurity d) <> " " <> write (t3NpcSneak d)
  <> " " <> write (t3NpcAcrobatics d) <> " " <> write (t3NpcLightArmor d) <> " " <> write (t3NpcShortBlade d) <> " " <> write (t3NpcMarksman d)
  <> " " <> write (t3NpcMercantile d) <> " " <> write (t3NpcSpeechcraft d) <> " " <> write (t3NpcHandToHand d) <> " " <> write (t3NpcFaction d)
  <> " " <> write (t3NpcHealth d) <> " " <> write (t3NpcMagicka d) <> " " <> write (t3NpcFatigue d)

writeT3Field :: T3Field -> Text
writeT3Field (T3BinaryField sign d) = write sign <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field (T3StringField sign s) = write sign <> " " <> writeNulledLine s
writeT3Field (T3MultilineField sign t) = write sign <> "\n" <> writeLines t
writeT3Field (T3MultiStringField sign t) = write sign <> " " <> writeNames t
writeT3Field (T3RefField sign z n) = write sign <> " " <> write z <> " " <> writeLine n
writeT3Field (T3FloatField sign v) = write sign <> " " <> either (("x" <>) . write) (write . float2Double) v <> "\n"
writeT3Field (T3IntField sign v) = write sign <> " " <> write v <> "\n"
writeT3Field (T3ShortField sign v) = write sign <> " " <> write v <> "\n"
writeT3Field (T3LongField sign v) = write sign <> " " <> write v <> "\n"
writeT3Field (T3ByteField sign v) = write sign <> " " <> write v <> "\n"
writeT3Field (T3CompressedField sign d) = write sign <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field
  ( T3IngredientField sign
    ( T3IngredientData weight value
      (T3IngredientEffects e1 e2 e3 e4)
      (T3IngredientSkills s1 s2 s3 s4)
      (T3IngredientAttributes a1 a2 a3 a4)
    )
  ) =
  write sign <> "\n"
    <> "    " <> T.pack (show weight) <> " " <> T.pack (show value) <> "\n"
    <> "    " <> write e1 <> " " <> write e2 <> " " <> write e3 <> " " <> T.pack (show e4) <> "\n"
    <> "    " <> write s1 <> " " <> write s2 <> " " <> write s3 <> " " <> write s4 <> "\n"
    <> "    " <> write a1 <> " " <> write a2 <> " " <> write a3 <> " " <> write a4 <> "\n"
writeT3Field
  ( T3ScriptField sign
    ( T3ScriptHeader name
      shorts longs floats
      data_size var_table_size
    )
  ) =
  write sign
    <> " " <> writeRun name
    <> " " <> write shorts <> " " <> write longs <> " " <> write floats
    <> " " <> write data_size <> " " <> write var_table_size
    <> "\n"
writeT3Field (T3DialField sign t) = write sign <> either (\x -> if x == 0 then T.empty else " " <> T.pack (show x)) ((" " <> ) . writeEnum 2) t <> "\n"
writeT3Field (T3NoneField sign) = write sign <> "\n"
writeT3Field (T3HeaderField sign (T3FileHeader version file_type author description))
  =  write sign <> " " <> writeEnum 0 file_type <> " " <> write version <> "\n"
  <> "    " <> writeLine author
  <> writeLines description
writeT3Field (T3EssNpcField sign (T3EssNpcData disposition reputation index))
  =  write sign
  <> " " <> write index
  <> " " <> write disposition
  <> " " <> write reputation
  <> "\n"
writeT3Field (T3NpcField sign (T3NpcData level disposition reputation rank gold ch)) =
  let
    char_text = case ch of
      Left x -> " (" <> write x <> ")"
      Right n -> writeNpcChar n
  in write sign
  <> " " <> write level
  <> " " <> write disposition
  <> " " <> write reputation
  <> " " <> write rank
  <> " " <> write gold
  <> char_text <> "\n"

writeT3Flags :: T3Flags -> Text
writeT3Flags f =
  ""
  <> (if t3Deleted f then " Deleted" else "")
  <> (if t3Blocked f then " Blocked" else "")
  <> (if t3Persist f then " Persist" else "")

writeT3Record :: T3Record -> Text
writeT3Record (T3Record sign fl fields)
  =  write sign <> writeT3Flags fl <> "\n"
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

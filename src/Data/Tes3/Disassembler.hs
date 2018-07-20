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

module Data.Tes3.Disassembler
  ( T3DisassemblerError (..)
  , disassembly
  ) where

#include <haskell>
import Data.Tes3
import Data.Tes3.Get
import Data.Tes3.Utils

genNpcChar :: T3NpcDataChar -> TextGen
genNpcChar d
  =  " " <> genShow (t3NpcStrength d) >> " " <> genShow (t3NpcIntelligence d) >> " " <> genShow (t3NpcWillpower d) >> " " <> genShow (t3NpcAgility d)
  >> " " <> genShow (t3NpcSpeed d) >> " " <> genShow (t3NpcEndurance d) >> " " <> genShow (t3NpcPersonality d) >> " " <> genShow (t3NpcLuck d)
  >> " " <> genShow (t3NpcBlock d) >> " " <> genShow (t3NpcArmorer d) >> " " <> genShow (t3NpcMediumArmor d) >> " " <> genShow (t3NpcHeavyArmor d)
  >> " " <> genShow (t3NpcBluntWeapon d) >> " " <> genShow (t3NpcLongBlade d) >> " " <> genShow (t3NpcAxe d) >> " " <> genShow (t3NpcSpear d)
  >> " " <> genShow (t3NpcAthletics d) >> " " <> genShow (t3NpcEnchant d) >> " " <> genShow (t3NpcDestruction d) >> " " <> genShow (t3NpcAlteration d)
  >> " " <> genShow (t3NpcIllusion d) >> " " <> genShow (t3NpcConjuration d) >> " " <> genShow (t3NpcMysticism d) >> " " <> genShow (t3NpcRestoration d)
  >> " " <> genShow (t3NpcAlchemy d) >> " " <> genShow (t3NpcUnarmored d) >> " " <> genShow (t3NpcSecurity d) >> " " <> genShow (t3NpcSneak d)
  >> " " <> genShow (t3NpcAcrobatics d) >> " " <> genShow (t3NpcLightArmor d) >> " " <> genShow (t3NpcShortBlade d) >> " " <> genShow (t3NpcMarksman d)
  >> " " <> genShow (t3NpcMercantile d) >> " " <> genShow (t3NpcSpeechcraft d) >> " " <> genShow (t3NpcHandToHand d) >> " " <> genShow (t3NpcFaction d)
  >> " " <> genShow (t3NpcHealth d) >> " " <> genShow (t3NpcMagicka d) >> " " <> genShow (t3NpcFatigue d)

genT3Field :: T3Field -> TextGen
genT3Field (T3BinaryField sign d) = genShow sign >> " " <> genLazyString (T.pack $ C.unpack $ encode d) >> "\n"
genT3Field (T3StringField sign s) = genShow sign >> " " <> genNulledLine s
genT3Field (T3MultilineField sign t) = genShow sign <> "\n" <> genLines t
genT3Field (T3MultiStringField sign t) = genShow sign >> " " <> genNames t
genT3Field (T3RefField sign z n) = genShow sign >> " " <> genShow z >> " " <> genLine n
genT3Field (T3FloatField sign v) = genShow sign >> " " <> either (("x" <>) . genShow) (genShow . float2Double) v >> "\n"
genT3Field (T3IntField sign v) = genShow sign >> " " <> genShow v >> "\n"
genT3Field (T3ShortField sign v) = genShow sign >> " " <> genShow v >> "\n"
genT3Field (T3LongField sign v) = genShow sign >> " " <> genShow v >> "\n"
genT3Field (T3ByteField sign v) = genShow sign >> " " <> genShow v >> "\n"
genT3Field (T3CompressedField sign d) = genShow sign >> " " <> genLazyString (T.pack $ C.unpack $ encode d) >> "\n"
genT3Field
  ( T3IngredientField sign
    ( T3IngredientData weight value
      (T3IngredientEffects e1 e2 e3 e4)
      (T3IngredientSkills s1 s2 s3 s4)
      (T3IngredientAttributes a1 a2 a3 a4)
    )
  )
   = genShow sign <> "\n"
  <> "    " >> genShow weight <> " " <> genShow value <> "\n"
  <> "    " >> genShow e1 <> " " <> genShow e2 <> " " <> genShow e3 <> " " <> genShow e4 <> "\n"
  <> "    " >> genShow s1 <> " " <> genShow s2 <> " " <> genShow s3 <> " " <> genShow s4 <> "\n"
  <> "    " >> genShow a1 <> " " <> genShow a2 <> " " <> genShow a3 <> " " <> genShow a4 <> "\n"
genT3Field
  ( T3ScriptField sign
    ( T3ScriptHeader name
      shorts longs floats
      data_size var_table_size
    )
  )
   = genShow sign
  <> " " <> genRun name
  <> " " <> genShow shorts >> " " <> genShow longs >> " " <> genShow floats
  <> " " <> genShow data_size >> " " <> genShow var_table_size
  <> "\n"
genT3Field (T3DialField sign t) =
  genShow sign >> either (\x -> if x == 0 then return () else " " <> genShow x) ((" " <>) . genEnum 2) t <> "\n"
genT3Field (T3NoneField sign) = genShow sign <> "\n"
genT3Field (T3HeaderField sign (T3FileHeader version file_type author description))
  =  genShow sign <> " " <> genEnum 0 file_type <> " " <> genShow version <> "\n"
  <> "    " >> genLine author
  >> genLines description
genT3Field (T3EssNpcField sign (T3EssNpcData disposition reputation index))
  =  genShow sign
  >> " " <> genShow index
  >> " " <> genShow disposition
  >> " " <> genShow reputation
  >> "\n"
genT3Field (T3NpcField sign (T3NpcData level disposition reputation rank gold ch)) =
  let
    char_text = case ch of
      Left x -> " (" <> genShow x <> ")"
      Right n -> genNpcChar n
  in genShow sign
  >> " " <> genShow level
  >> " " <> genShow disposition
  >> " " <> genShow reputation
  >> " " <> genShow rank
  >> " " <> genShow gold
  >> char_text >> "\n"

genT3Flags :: T3Flags -> TextGen
genT3Flags f
  =  genString (if t3Deleted f then " Deleted" else "")
  >> genString (if t3Blocked f then " Blocked" else "")
  >> genString (if t3Persist f then " Persist" else "")

genT3Record :: T3Record -> TextGen
genT3Record (T3Record sign fl fields)
  =  genShow sign <> genT3Flags fl <> "\n"
  <> forM_ fields genT3Field

data T3DisassemblerError = T3Error !T3Error | T3RecordsCountMismatch !Word32 !Word32

instance Show T3DisassemblerError where
  show (T3Error !e) = show e
  show (T3RecordsCountMismatch expected actual) = concat ["Records count mismatch: no more than ", shows expected " expected, but ", shows actual " readed."]

disassembly :: Monad m => Bool -> (T3Sign -> Bool) -> Word32 -> ConduitT S.ByteString S.Text m (Either T3DisassemblerError ())
disassembly adjust skip_record items_count = runGet $ do
  let
    write_rec first (T3Record s a b) =
      if skip_record s
        then return ()
        else (if first then return () else "\n") >> genT3Record (T3Record s a b)
  first_rec <- mapError T3Error (getT3Record adjust)
  runTextGen $ write_rec True first_rec
  (_, !n) <- (flip runStateT) 0 $ whileM_ (not <$> lift N.nullE) $ do
    rec <- lift $ mapError T3Error (getT3Record adjust)
    lift $ runTextGen $ write_rec False rec
    modify' (+ 1)
  if n > items_count
    then throwError $ T3RecordsCountMismatch items_count n
    else return ()
  if n /= items_count
    then runTextGen $ "\n# " <> genShow items_count <> "\n"
    else return ()

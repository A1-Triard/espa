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

module Data.Tes3.Parser
  ( pT3Record
  ) where

#include <haskell>
import Data.Tes3
import Data.Tes3.Utils

pT3Flag :: Parser () (T3Flags -> T3Flags)
pT3Flag
   =  (pStringIs "Blocked" >> return (\f -> f { t3Blocked = True }))
  <|> (pStringIs "Persist" >> return (\f -> f { t3Persist = True }))
  <|> (pStringIs "Deleted" >> return (\f -> f { t3Deleted = True }))

pT3Flags :: Parser () T3Flags
pT3Flags = do
  t <- whileM (maybe False (const True) <$> option'' (pCharIs ' ')) pT3Flag
  return $ foldr ($) t3FlagsEmpty t

pT3Record :: Parser () T3Record
pT3Record = do
  s <- pT3Sign
  g <- pT3Flags
  skipEndOfLine
  fields <- many'' $ t3Field s
  return $ T3Record s g fields

t3Field :: T3Sign -> Parser (Maybe String) T3Field
t3Field record_sign = do
  s <- mapError (const Nothing) pT3Sign
  t3FieldBody (t3FieldType record_sign s) s

pFloat :: Parser () Float
pFloat = double2Float <$> pDouble <|> const (0/0) <$> pStringIs "NaN"

pNpcChar :: Parser () T3NpcDataChar
pNpcChar = do
  strength <- pDecimal
  skipCharIs ' '
  intelligence <- pDecimal
  skipCharIs ' '
  willpower <- pDecimal
  skipCharIs ' '
  agility <- pDecimal
  skipCharIs ' '
  speed <- pDecimal
  skipCharIs ' '
  endurance <- pDecimal
  skipCharIs ' '
  personality <- pDecimal
  skipCharIs ' '
  luck <- pDecimal
  skipCharIs ' '
  block <- pDecimal
  skipCharIs ' '
  armorer <- pDecimal
  skipCharIs ' '
  mediumArmor <- pDecimal
  skipCharIs ' '
  heavyArmor <- pDecimal
  skipCharIs ' '
  bluntWeapon <- pDecimal
  skipCharIs ' '
  longBlade <- pDecimal
  skipCharIs ' '
  axe <- pDecimal
  skipCharIs ' '
  spear <- pDecimal
  skipCharIs ' '
  athletics <- pDecimal
  skipCharIs ' '
  enchant <- pDecimal
  skipCharIs ' '
  destruction <- pDecimal
  skipCharIs ' '
  alteration <- pDecimal
  skipCharIs ' '
  illusion <- pDecimal
  skipCharIs ' '
  conjuration <- pDecimal
  skipCharIs ' '
  mysticism <- pDecimal
  skipCharIs ' '
  restoration <- pDecimal
  skipCharIs ' '
  alchemy <- pDecimal
  skipCharIs ' '
  unarmored <- pDecimal
  skipCharIs ' '
  security <- pDecimal
  skipCharIs ' '
  sneak <- pDecimal
  skipCharIs ' '
  acrobatics <- pDecimal
  skipCharIs ' '
  lightArmor <- pDecimal
  skipCharIs ' '
  shortBlade <- pDecimal
  skipCharIs ' '
  marksman <- pDecimal
  skipCharIs ' '
  mercantile <- pDecimal
  skipCharIs ' '
  speechcraft <- pDecimal
  skipCharIs ' '
  handToHand <- pDecimal
  skipCharIs ' '
  faction <- pDecimal
  skipCharIs ' '
  health <- pSignedDecimal
  skipCharIs ' '
  magicka <- pSignedDecimal
  skipCharIs ' '
  fatigue <- pSignedDecimal
  return $ T3NpcDataChar
    strength intelligence willpower agility speed endurance personality luck block armorer mediumArmor heavyArmor
    bluntWeapon longBlade axe spear athletics enchant destruction alteration illusion conjuration mysticism restoration
    alchemy unarmored security sneak acrobatics lightArmor shortBlade marksman mercantile speechcraft handToHand faction
    health magicka fatigue

t3FieldBody :: T3FieldType -> T3Sign -> Parser (Maybe String) T3Field
t3FieldBody T3Binary s = do
  skipCharIs ' ' ?>> return Nothing
  b <- decode <$> C.pack <$> ST.unpack <$> pStringTill isEndOfLine
  skipEndOfLine ?>> return Nothing
  case b of
    Left e -> throwError $ Just e
    Right r -> return $ T3BinaryField s r
t3FieldBody (T3String _) s = do
  skipCharIs ' ' ?>> return Nothing
  t <- pNulledLine ?>> return Nothing
  return $ T3StringField s t
t3FieldBody (T3Multiline _ _) s = do
  skipEndOfLine ?>> return Nothing
  t <- pLines ?>> return Nothing
  return $ T3MultilineField s t
t3FieldBody T3MultiString s = do
  skipCharIs ' ' ?>> return Nothing
  t <- pNames ?>> return Nothing
  return $ T3MultiStringField s t
t3FieldBody T3Ref s = do
  skipCharIs ' ' ?>> return Nothing
  n <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  t <- pLine ?>> return Nothing
  return $ T3RefField s n t
t3FieldBody (T3FixedString _) s = do
  skipCharIs ' ' ?>> return Nothing
  t <- pNulledLine ?>> return Nothing
  return $ T3StringField s t
t3FieldBody T3Float s = do
  skipCharIs ' ' ?>> return Nothing
  isNonStandardNan <- maybe False (const True) <$> option'' (pCharIs 'x')
  v <- (if isNonStandardNan then Left <$> pDecimal else Right <$> pFloat) ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3FloatField s v
t3FieldBody T3Int s = do
  skipCharIs ' ' ?>> return Nothing
  v <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3IntField s v
t3FieldBody T3Short s = do
  skipCharIs ' ' ?>> return Nothing
  v <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3ShortField s v
t3FieldBody T3Long s = do
  skipCharIs ' ' ?>> return Nothing
  v <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3LongField s v
t3FieldBody T3Byte s = do
  skipCharIs ' ' ?>> return Nothing
  v <- pDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3ByteField s v
t3FieldBody T3Compressed s = do
  skipCharIs ' ' ?>> return Nothing
  b <- decode <$> C.pack <$> ST.unpack <$> pStringTill isEndOfLine ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  case b of
    Left e -> throwError $ Just e
    Right r -> return $ T3CompressedField s r
t3FieldBody T3Ingredient s = do
  skipEndOfLine ?>> return Nothing
  skipStringIs "    " ?>> return Nothing
  weight <- pFloat ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  value <- pDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  skipStringIs "    " ?>> return Nothing
  e1 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  e2 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  e3 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  e4 <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  skipStringIs "    " ?>> return Nothing
  s1 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  s2 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  s3 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  s4 <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  skipStringIs "    " ?>> return Nothing
  a1 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  a2 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  a3 <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  a4 <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3IngredientField s $ T3IngredientData
    weight value
    (T3IngredientEffects e1 e2 e3 e4)
    (T3IngredientSkills s1 s2 s3 s4)
    (T3IngredientAttributes a1 a2 a3 a4)
t3FieldBody T3Script s = do
  skipCharIs ' ' ?>> return Nothing
  name <- pRun ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  shorts <- pDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  longs <- pDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  floats <- pDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  data_size <- pDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  var_table_size <- pDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3ScriptField s $ T3ScriptHeader
    name
    shorts longs floats
    data_size var_table_size
t3FieldBody T3Dial s = mapError (const Nothing) $ do
  t
     <- (skipEndOfLine >> return (Left 0))
    <|> (pCharIs ' ' >> Left <$> pDecimal <* skipEndOfLine)
    <|> (pCharIs ' ' >> Right <$> pEnum 2 <* skipEndOfLine)
  return $ T3DialField s t
t3FieldBody T3None s = do
  skipEndOfLine ?>> return Nothing
  return $ T3NoneField s
t3FieldBody T3Header s = do
  skipCharIs ' ' ?>> return Nothing
  file_type <- pEnum 0 ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  version <- pDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  skipStringIs "    " ?>> return Nothing
  author <- pLine ?>> return Nothing
  description <- pLines ?>> return Nothing
  return $ T3HeaderField s $ T3FileHeader version file_type author description
t3FieldBody T3EssNpc s = do
  skipCharIs ' ' ?>> return Nothing
  index <- pDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  disposition <- pSignedDecimal ?>> return Nothing
  skipCharIs ' ' ?>> return Nothing
  reputation <- pSignedDecimal ?>> return Nothing
  skipEndOfLine ?>> return Nothing
  return $ T3EssNpcField s $ T3EssNpcData disposition reputation index
t3FieldBody T3Npc s = mapError (const Nothing) $ do
  skipCharIs ' '
  level <- pDecimal
  skipCharIs ' '
  disposition <- pSignedDecimal
  skipCharIs ' '
  reputation <- pSignedDecimal
  skipCharIs ' '
  rank <- pSignedDecimal
  skipCharIs ' '
  gold <- pSignedDecimal
  skipCharIs ' '
  ch <- (Left <$> (pCharIs '(' *> pDecimal <* pCharIs ')')) <|> (Right <$> pNpcChar)
  skipEndOfLine
  return $ T3NpcField s $ T3NpcData level disposition reputation rank gold ch

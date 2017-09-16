module Data.Tes3.Parser.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Utils

pT3Flag :: T.Parser (T3Flags -> T3Flags)
pT3Flag
   =  (Tp.string "Blocked" >> return (\f -> f { t3Blocked = True }))
  <|> (Tp.string "Persist" >> return (\f -> f { t3Persist = True }))
  <|> (Tp.string "Deleted" >> return (\f -> f { t3Deleted = True }))

pT3Flags :: T.Parser T3Flags
pT3Flags = do
  t <- whileM (Tp.option False $ Tp.char ' ' >> return True) pT3Flag
  return $ foldr ($) t3FlagsEmpty t

pT3Record :: T.Parser T3Record
pT3Record = do
  s <- pT3Sign
  g <- pT3Flags
  Tp.endOfLine
  fields <- many $ t3Field s
  return $ T3Record s g fields

t3Field :: T3Sign -> T.Parser T3Field
t3Field record_sign = do
  s <- pT3Sign
  t3FieldBody (t3FieldType record_sign s) s

pFloat :: T.Parser Float
pFloat = (double2Float <$> Tp.double) <|> (const (0/0) <$> Tp.string "NaN")

pNpcChar :: T.Parser T3NpcDataChar
pNpcChar = do
  void $ Tp.char ' '
  strength <- Tp.decimal
  void $ Tp.char ' '
  intelligence <- Tp.decimal
  void $ Tp.char ' '
  willpower <- Tp.decimal
  void $ Tp.char ' '
  agility <- Tp.decimal
  void $ Tp.char ' '
  speed <- Tp.decimal
  void $ Tp.char ' '
  endurance <- Tp.decimal
  void $ Tp.char ' '
  personality <- Tp.decimal
  void $ Tp.char ' '
  luck <- Tp.decimal
  void $ Tp.char ' '
  block <- Tp.decimal
  void $ Tp.char ' '
  armorer <- Tp.decimal
  void $ Tp.char ' '
  mediumArmor <- Tp.decimal
  void $ Tp.char ' '
  heavyArmor <- Tp.decimal
  void $ Tp.char ' '
  bluntWeapon <- Tp.decimal
  void $ Tp.char ' '
  longBlade <- Tp.decimal
  void $ Tp.char ' '
  axe <- Tp.decimal
  void $ Tp.char ' '
  spear <- Tp.decimal
  void $ Tp.char ' '
  athletics <- Tp.decimal
  void $ Tp.char ' '
  enchant <- Tp.decimal
  void $ Tp.char ' '
  destruction <- Tp.decimal
  void $ Tp.char ' '
  alteration <- Tp.decimal
  void $ Tp.char ' '
  illusion <- Tp.decimal
  void $ Tp.char ' '
  conjuration <- Tp.decimal
  void $ Tp.char ' '
  mysticism <- Tp.decimal
  void $ Tp.char ' '
  restoration <- Tp.decimal
  void $ Tp.char ' '
  alchemy <- Tp.decimal
  void $ Tp.char ' '
  unarmored <- Tp.decimal
  void $ Tp.char ' '
  security <- Tp.decimal
  void $ Tp.char ' '
  sneak <- Tp.decimal
  void $ Tp.char ' '
  acrobatics <- Tp.decimal
  void $ Tp.char ' '
  lightArmor <- Tp.decimal
  void $ Tp.char ' '
  shortBlade <- Tp.decimal
  void $ Tp.char ' '
  marksman <- Tp.decimal
  void $ Tp.char ' '
  mercantile <- Tp.decimal
  void $ Tp.char ' '
  speechcraft <- Tp.decimal
  void $ Tp.char ' '
  handToHand <- Tp.decimal
  void $ Tp.char ' '
  faction <- Tp.decimal
  void $ Tp.char ' '
  health <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  magicka <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  fatigue <- Tp.signed Tp.decimal
  return $ T3NpcDataChar
    strength intelligence willpower agility speed endurance personality luck block armorer mediumArmor heavyArmor
    bluntWeapon longBlade axe spear athletics enchant destruction alteration illusion conjuration mysticism restoration
    alchemy unarmored security sneak acrobatics lightArmor shortBlade marksman mercantile speechcraft handToHand faction
    health magicka fatigue

t3FieldBody :: T3FieldType -> T3Sign -> T.Parser T3Field
t3FieldBody T3Binary s = do
  void $ Tp.char ' '
  b <- decode <$> C.pack <$> ST.unpack <$> Tp.takeTill Tp.isEndOfLine
  Tp.endOfLine
  case b of
    Left e -> fail e
    Right r -> return $ T3BinaryField s r
t3FieldBody (T3String _) s = do
  void $ Tp.char ' '
  t <- pNulledLine
  return $ T3StringField s t
t3FieldBody (T3Multiline _ _) s = do
  Tp.endOfLine
  t <- pLines
  return $ T3MultilineField s t
t3FieldBody T3MultiString s = do
  void $ Tp.char ' '
  t <- pNames
  return $ T3MultiStringField s t
t3FieldBody T3Ref s = do
  void $ Tp.char ' '
  n <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  t <- pLine
  return $ T3RefField s n t
t3FieldBody (T3FixedString _) s = do
  void $ Tp.char ' '
  t <- pNulledLine
  return $ T3StringField s t
t3FieldBody T3Float s = do
  void $ Tp.char ' '
  isNonStandardNan <- Tp.option False (Tp.char 'x' >> return True)
  v <- if isNonStandardNan then Left <$> Tp.decimal else Right <$> pFloat
  Tp.endOfLine
  return $ T3FloatField s v
t3FieldBody T3Int s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3IntField s v
t3FieldBody T3Short s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3ShortField s v
t3FieldBody T3Long s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3LongField s v
t3FieldBody T3Byte s = do
  void $ Tp.char ' '
  v <- Tp.decimal
  Tp.endOfLine
  return $ T3ByteField s v
t3FieldBody T3Compressed s = do
  void $ Tp.char ' '
  b <- decode <$> C.pack <$> ST.unpack <$> Tp.takeTill Tp.isEndOfLine
  Tp.endOfLine
  case b of
    Left e -> fail e
    Right r -> return $ T3CompressedField s r
t3FieldBody T3Ingredient s = do
  Tp.endOfLine
  void $ Tp.string "    "
  weight <- pFloat
  void $ Tp.char ' '
  value <- Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  e1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  s1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  a1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3IngredientField s $ T3IngredientData
    weight value
    (T3IngredientEffects e1 e2 e3 e4)
    (T3IngredientSkills s1 s2 s3 s4)
    (T3IngredientAttributes a1 a2 a3 a4)
t3FieldBody T3Script s = do
  void $ Tp.char ' '
  name <- pRun
  void $ Tp.char ' '
  shorts <- Tp.decimal
  void $ Tp.char ' '
  longs <- Tp.decimal
  void $ Tp.char ' '
  floats <- Tp.decimal
  void $ Tp.char ' '
  data_size <- Tp.decimal
  void $ Tp.char ' '
  var_table_size <- Tp.decimal
  Tp.endOfLine
  return $ T3ScriptField s $ T3ScriptHeader
    name
    shorts longs floats
    data_size var_table_size
t3FieldBody T3Dial s = do
  void $ Tp.char ' '
  t <- (Tp.endOfLine >> return (Left 0)) <|> (Left <$> Tp.decimal <* Tp.endOfLine) <|> (Right <$> pEnum 2 <* Tp.endOfLine)
  return $ T3DialField s t
t3FieldBody T3None s = do
  Tp.endOfLine
  return $ T3NoneField s
t3FieldBody T3Header s = do
  void $ Tp.char ' '
  file_type <- pEnum 0
  void $ Tp.char ' '
  version <- Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  author <- pLine
  description <- pLines
  return $ T3HeaderField s $ T3FileHeader version file_type author description
t3FieldBody T3EssNpc s = do
  void $ Tp.char ' '
  index <- Tp.decimal
  void $ Tp.char ' '
  disposition <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  reputation <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3EssNpcField s $ T3EssNpcData disposition reputation index
t3FieldBody T3Npc s = do
  void $ Tp.char ' '
  level <- Tp.decimal
  void $ Tp.char ' '
  disposition <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  reputation <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  rank <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  gold <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  unknown <- Tp.decimal
  ch <- Tp.option Nothing (Just <$> pNpcChar)
  Tp.endOfLine
  return $ T3NpcField s $ T3NpcData level disposition reputation rank gold ch unknown

module Data.Tes3.Native where

#include <haskell>

data T3Mark
  = TES3 | HEDR | MAST | DATA | GLOB | NAME | FNAM | FORM
  | GMST | GMDT | ACTI | ALCH | APPA | ARMO | BODY | BOOK
  | BSGN | CELL | CLAS | CLOT | CNTC | CONT | CREA | CREC
  | DIAL | DOOR | ENCH | FACT | INFO | INGR | LAND | LEVC
  | LEVI | LIGH | LOCK | LTEX | MGEF | MISC | NPC_ | NPCC
  | PGRD | PROB | RACE | REGN | REPA | SCPT | SKIL | SNDG
  | SOUN | SPEL | SSCR | STAT | WEAP | SAVE | JOUR | QUES
  | GSCR | PLAY | CSTA | GMAP | DIAS | WTHR | KEYS | DYNA
  | ASPL | ACTC | MPRJ | PROJ | DCOU | MARK | FILT | DBGP
  | STRV | INTV | FLTV | SCHD | SCVR | SCDT | SCTX | MODL
  | IRDT | SCRI | ITEX | CNDT | FLAG | MCDT | NPCO | AADT
  | CTDT | RNAM | INDX | CNAM | ANAM | BNAM | KNAM | NPDT
  | AIDT | DODT | AI_W | CLDT | BYDT | RGNN | AODT | NAM5
  | DESC | WHGT | FADT | AMBI | FRMR | RADT | NAM0 | NPCS
  | DNAM | XSCL | SKDT | DELE | MEDT | PTEX | CVFX | BVFX
  | HVFX | AVFX | BSND | CSND | HSND | ASND | WEAT | SNAM
  | INAM | NNAM | PNAM | ONAM | TNAM | ENAM | TEXT | VNML
  | VHGT | VCLR | VTEX | WNAM
  deriving (Eq, Ord, Enum, Bounded, Show)

data T3Sign = T3Mark T3Mark | T3Sign Word32 deriving Eq

instance Show T3Sign where
  show (T3Mark m) = show m
  show (T3Sign s) =
    let (a, b, c, d) = toBytes s in
    [chr $ fromIntegral a, chr $ fromIntegral b, chr $ fromIntegral c, chr $ fromIntegral d]

pT3Sign :: T.Parser T3Sign
pT3Sign = do
  a <- Tp.anyChar
  b <- Tp.anyChar
  c <- Tp.anyChar
  d <- Tp.anyChar
  return $ t3SignNew $ fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)

fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fromBytes a b c d
  =  (fromIntegral a)
  .|. (shift (fromIntegral b) 8)
  .|. (shift (fromIntegral c) 16)
  .|. (shift (fromIntegral d) 24)
  
toBytes :: Word32 -> (Word8, Word8, Word8, Word8)
toBytes w =
  ( fromIntegral $ w .&. 0xFF
  , fromIntegral $ (.&. 0xFF) $ shift w (-8)
  , fromIntegral $ (.&. 0xFF) $ shift w (-16)
  , fromIntegral $ (.&. 0xFF) $ shift w (-24)
  )

t3MarkValueSlow :: T3Mark -> Word32
t3MarkValueSlow mark =
  calc (show mark)
  where
    calc [a, b, c, d] = fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)
    calc _ = 0x0BAD0BAD

t3MarkValues :: S.Map T3Mark Word32
t3MarkValues = SM.fromList [(m, t3MarkValueSlow m) | m <- [minBound .. maxBound]]

t3MarkValue :: T3Mark -> Word32
t3MarkValue m = fromMaybe 0x0BAD0BAD $ SM.lookup m t3MarkValues

t3SignValue :: T3Sign -> Word32
t3SignValue (T3Mark m) = t3MarkValue m
t3SignValue (T3Sign s) = s

t3MarkNews :: S.Map Word32 T3Mark
t3MarkNews = SM.fromList [(t3MarkValue m, m) | m <- [minBound .. maxBound]]

t3MarkNew :: Word32 -> Maybe T3Mark
t3MarkNew w = SM.lookup w t3MarkNews

t3SignNew :: Word32 -> T3Sign
t3SignNew w = fromMaybe (T3Sign w) $ T3Mark <$> t3MarkNew w

data T3FileType = ESP | ESM | ESS deriving (Eq, Enum, Show, Bounded)

t3FileTypeValue :: T3FileType -> Word32
t3FileTypeValue ESP = 0
t3FileTypeValue ESM = 1
t3FileTypeValue ESS = 32

t3FileTypeNew :: Word32 -> Maybe T3FileType
t3FileTypeNew 0 = Just ESP
t3FileTypeNew 1 = Just ESM
t3FileTypeNew 32 = Just ESS
t3FileTypeNew _ = Nothing

pT3FileType :: T.Parser T3FileType
pT3FileType = foldl1 (<|>) [Tp.string (ST.pack $ show t) >> return t | t <- [minBound .. maxBound]]

data T3FieldType
  = T3Binary
  | T3String (Text -> Text)
  | T3Multiline (Text -> Text)
  | T3MultiString
  | T3Ref
  | T3FixedString Word32
  | T3Float
  | T3Int
  | T3Short
  | T3Long
  | T3Byte
  | T3Compressed
  | T3Ingredient
  | T3Script

t3FieldType :: T3Sign -> T3Sign -> T3FieldType
t3FieldType (T3Mark NPC_) (T3Mark ANAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark ANAM) = T3String id
t3FieldType _ (T3Mark ASND) = T3String id
t3FieldType _ (T3Mark AVFX) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark BNAM) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark BODY) (T3Mark BNAM) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark CLOT) (T3Mark BNAM) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark CONT) (T3Mark BNAM) = T3Multiline $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark NPC_) (T3Mark BNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType (T3Mark REGN) (T3Mark BNAM) = T3String id
t3FieldType _ (T3Mark BNAM) = T3Multiline id
t3FieldType _ (T3Mark BSND) = T3String id
t3FieldType _ (T3Mark BVFX) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark CNAM) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark NPC_) (T3Mark CNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType (T3Mark REGN) (T3Mark CNAM) = T3Int
t3FieldType _ (T3Mark CNAM) = T3String id
t3FieldType _ (T3Mark CSND) = T3String id
t3FieldType _ (T3Mark CVFX) = T3String id
t3FieldType (T3Mark DIAL) (T3Mark DATA) = T3Byte
t3FieldType (T3Mark LEVC) (T3Mark DATA) = T3Int
t3FieldType (T3Mark LEVI) (T3Mark DATA) = T3Int
t3FieldType (T3Mark LTEX) (T3Mark DATA) = T3String id
t3FieldType (T3Mark SSCR) (T3Mark DATA) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark DESC) = T3String id
t3FieldType _ (T3Mark DNAM) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark ENAM) = T3String id
t3FieldType _ (T3Mark FLAG) = T3Int
t3FieldType _ (T3Mark FLTV) = T3Float
t3FieldType (T3Mark ACTI) (T3Mark FNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType (T3Mark RACE) (T3Mark FNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark FNAM) = T3String id
t3FieldType _ (T3Mark HSND) = T3String id
t3FieldType _ (T3Mark HVFX) = T3String id
t3FieldType _ (T3Mark INAM) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark INDX) = T3Byte
t3FieldType (T3Mark CLOT) (T3Mark INDX) = T3Byte
t3FieldType _ (T3Mark INDX) = T3Int
t3FieldType (T3Mark LAND) (T3Mark INTV) = T3Long
t3FieldType (T3Mark LEVC) (T3Mark INTV) = T3Short
t3FieldType (T3Mark LEVI) (T3Mark INTV) = T3Short
t3FieldType _ (T3Mark INTV) = T3Int
t3FieldType _ (T3Mark ITEX) = T3String id
t3FieldType (T3Mark NPC_) (T3Mark KNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark KNAM) = T3String id
t3FieldType (T3Mark LIGH) (T3Mark MODL) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark MODL) = T3String id
t3FieldType (T3Mark CELL) (T3Mark NAME) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType (T3Mark SSCR) (T3Mark NAME) = T3String $ T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark NAME) = T3String id
t3FieldType (T3Mark INFO) (T3Mark NNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType (T3Mark LEVC) (T3Mark NNAM) = T3Byte
t3FieldType (T3Mark LEVI) (T3Mark NNAM) = T3Byte
t3FieldType _ (T3Mark NNAM) = T3String id
t3FieldType _ (T3Mark NPCO) = T3Ref
t3FieldType (T3Mark BSGN) (T3Mark NPCS) = T3FixedString 32
t3FieldType (T3Mark RACE) (T3Mark NPCS) = T3FixedString 32
t3FieldType _ (T3Mark NPCS) = T3String id
t3FieldType _ (T3Mark ONAM) = T3String id
t3FieldType (T3Mark INFO) (T3Mark PNAM) = T3String $ (`T.snoc` '\0') . T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark PNAM) = T3String id
t3FieldType _ (T3Mark PTEX) = T3String id
t3FieldType _ (T3Mark RGNN) = T3String id
t3FieldType (T3Mark FACT) (T3Mark RNAM) = T3FixedString 32
t3FieldType _ (T3Mark RNAM) = T3String id
t3FieldType (T3Mark SCPT) (T3Mark SCHD) = T3Script
t3FieldType _ (T3Mark SCRI) = T3String id
t3FieldType _ (T3Mark SCTX) = T3Multiline $ T.dropWhileEnd (== '\0')
t3FieldType (T3Mark SCPT) (T3Mark SCVR) = T3MultiString
t3FieldType _ (T3Mark SCVR) = T3String id
t3FieldType (T3Mark REGN) (T3Mark SNAM) = T3Binary
t3FieldType _ (T3Mark SNAM) = T3String id
t3FieldType _ (T3Mark STRV) = T3String id
t3FieldType (T3Mark ALCH) (T3Mark TEXT) = T3String id
t3FieldType (T3Mark BOOK) (T3Mark TEXT) = T3Multiline $ T.dropWhileEnd (== '\0')
t3FieldType _ (T3Mark TEXT) = T3Multiline id
t3FieldType _ (T3Mark TNAM) = T3String id
t3FieldType _ (T3Mark VCLR) = T3Compressed
t3FieldType _ (T3Mark VHGT) = T3Compressed
t3FieldType _ (T3Mark VNML) = T3Compressed
t3FieldType _ (T3Mark VTEX) = T3Compressed
t3FieldType _ (T3Mark WEAT) = T3Binary
t3FieldType _ (T3Mark WNAM) = T3Compressed
t3FieldType (T3Mark INGR) (T3Mark IRDT) = T3Ingredient
t3FieldType _ _ = T3Binary

data T3IngredientEffects = T3IngredientEffects Int32 Int32 Int32 Int32 deriving (Eq, Show)
data T3IngredientSkills = T3IngredientSkills Int32 Int32 Int32 Int32 deriving (Eq, Show)
data T3IngredientAttributes = T3IngredientAttributes Int32 Int32 Int32 Int32 deriving (Eq, Show)

data T3IngredientData = T3IngredientData Float Word32 T3IngredientEffects T3IngredientSkills T3IngredientAttributes deriving (Eq, Show)
data T3ScriptHeader = T3ScriptHeader Text Word32 Word32 Word32 Word32 Word32 deriving (Eq, Show)

data T3Field
  = T3BinaryField T3Sign ByteString
  | T3StringField T3Sign Text
  | T3FixedStringField T3Sign Text
  | T3MultilineField T3Sign [Text]
  | T3MultiStringField T3Sign [Text]
  | T3RefField T3Sign Word32 Text
  | T3FloatField T3Sign Float
  | T3IntField T3Sign Int32
  | T3ShortField T3Sign Int16
  | T3LongField T3Sign Int64
  | T3ByteField T3Sign Word8
  | T3CompressedField T3Sign ByteString
  | T3IngredientField T3Sign T3IngredientData
  | T3ScriptField T3Sign T3ScriptHeader
  deriving (Eq, Show)
data T3Record = T3Record T3Sign Word64 [T3Field] deriving (Eq, Show)
data T3FileRef = T3FileRef Text Word64 deriving (Eq, Show)
data T3FileHeader = T3FileHeader Word32 T3FileType Text [Text] [T3FileRef] deriving (Eq, Show)

t3StringValue :: Text -> ByteString
t3StringValue = IC.convertFuzzy IC.Transliterate "UTF-8" "CP1251" . T.encodeUtf8

t3StringNew :: ByteString -> Text
t3StringNew = T.decodeUtf8 . IC.convert "CP1251" "UTF-8"

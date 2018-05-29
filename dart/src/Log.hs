module Log (
    decodeLog,
    DRTLog,
    DRTEntity (..)
) where

import           Control.Monad
import           Data.Binary.Get
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Int
import           Data.Word
import           Entities

data EntityType = FuncEntity | DataEntity | TransformEntity | MediumEntity deriving (Enum, Show)

data DRTEntity = DRTFunc Func | DRTData Data | DRTTransform Transform | DRTMedium Medium deriving (Show)

data ArgType = BlobArgType | InlineArgType deriving (Enum)

type DRTLog = [DRTEntity]

getEntity :: Get DRTEntity
getEntity = do
        entity <- toEnum . fromIntegral <$> getWord8
        case entity of
            FuncEntity      -> DRTFunc <$> decodeFunc
            DataEntity      -> DRTData <$> decodeData
            TransformEntity -> DRTTransform <$> decodeTransform
            MediumEntity    -> DRTMedium <$> decodeMedium

decodeLog :: BL.ByteString -> DRTLog
decodeLog b | BL.null b = []
decodeLog b = case runGetOrFail getEntity b of
        Left  (_, _, _)        -> []
        Right (leftover, _, a) -> a : decodeLog leftover

getDrtId :: Get ID
getDrtId = fromIntegral <$> getInt64be

getDrtLen :: Get Int
getDrtLen = fromIntegral <$> getInt32be

getDrtOffset :: Get Int64
getDrtOffset = fromIntegral <$> getInt64be

decodeTags :: Get [Tag]
decodeTags = do
    count <- getDrtLen
    replicateM count (do
            len <- getDrtLen
            string <- getByteString len
            return $ unpack string
        )

decodeMedium = liftM2 Medium getDrtId decodeTags
decodeData =  liftM5 Data getDrtId getDrtId getDrtOffset (fromIntegral <$> getWord32be) decodeTags
decodeBlob = liftM4 Blob getDrtId getDrtId getDrtOffset getDrtOffset
decodeFunc = do
    i <- getDrtId
    l <- getDrtLen
    b <- getByteString l
    return $ Func i (unpack b)

decodeTrans :: Get Trans
decodeTrans = do
    al <- getDrtLen
    a <- replicateM al (do
            t <- getWord8
            case toEnum (fromIntegral t) of
                BlobArgType -> BlobArg <$> getDrtId
                InlineArgType -> InlineArg <$> (do
                    l <- getDrtLen
                    b <- getByteString l
                    return $ unpack b
                    )
        )
    f <- getDrtId
    ol <- getDrtLen
    o <- replicateM ol getDrtId
    return $ Trans a f o

decodeTransform = do
    ty <- getWord8
    let t = toEnum $ fromIntegral ty
    ol <- getDrtLen
    o <- replicateM ol decodeBlob
    tr <- case t of
        Discard      -> return Nothing
        Irreversible -> return Nothing
        Reversible   -> Just <$> decodeTrans
        Lossy        -> Just <$> decodeTrans
    return $ Transform t o tr

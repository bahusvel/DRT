module Entities (
    getEntity,
    DRTEntity (..),
    Medium,
    Data (iblobId, dataId, dataTags),
    Blob,
    Func,
    Transform(Transform),
    Trans (out, args),
    Arg(BlobArg),
    BlobID,
    DataID,
    DRTLog
) where

import           Control.Monad
import           Data.Binary.Get
import           Data.ByteString.Char8 (unpack)
import           Data.Word

type ID = Int
type BlobID = ID
type DataID = ID
type MediumID = ID
type FuncID = ID
type Tag = String
type Checksum = Int

data EntityType = FuncEntity | DataEntity | TransformEntity | MediumEntity deriving (Enum, Show)

data DRTEntity = DRTFunc Func | DRTData Data | DRTTransform Transform | DRTMedium Medium deriving (Show)

type DRTLog = [DRTEntity]

getEntity :: Get DRTEntity
getEntity = do
        entity <- toEnum . fromIntegral <$> getWord8
        case entity of
            FuncEntity      -> DRTFunc <$> decodeFunc
            DataEntity      -> DRTData <$> decodeData
            TransformEntity -> DRTTransform <$> decodeTransform
            MediumEntity    -> DRTMedium <$> decodeMedium

getDrtId :: Get ID
getDrtId = fromIntegral <$> getInt64be

getDrtLen :: Get Int
getDrtLen = fromIntegral <$> getInt32be

getDrtOffset :: Get Int
getDrtOffset = fromIntegral <$> getInt64be

decodeTags :: Get [Tag]
decodeTags = do
    count <- getDrtLen
    replicateM count (do
            len <- getDrtLen
            string <- getByteString len
            return $ unpack string
        )

data Medium = Medium {
    mediumId   :: MediumID,
    mediumTags :: [Tag]
} deriving (Show)

decodeMedium = liftM2 Medium getDrtId decodeTags

data Data = Data {
    dataId   :: DataID,
    iblobId  :: BlobID, -- This is for first implicit data blob
    size     :: Int,
    checksum :: Checksum,
    dataTags :: [Tag]
} deriving (Show)

decodeData =  liftM5 Data getDrtId getDrtId getDrtOffset (fromIntegral <$> getWord32be) decodeTags

data Blob = Blob {
    blobId     :: BlobID,
    medium     :: MediumID,
    offset     :: Int,
    blobLength :: Int
} deriving (Show)

decodeBlob = liftM4 Blob getDrtId getDrtId getDrtOffset getDrtOffset

data Func = Func {
    funcId :: FuncID,
    code   :: String
} deriving (Show)


decodeFunc = do
    i <- getDrtId
    l <- getDrtLen
    b <- getByteString l
    return $ Func i (unpack b)

data ArgType = BlobArgType | InlineArgType deriving (Enum)
data Arg = BlobArg BlobID | InlineArg String deriving (Show)

data Trans = Trans {
    args :: [Arg],
    func :: FuncID,
    out  :: [BlobID]
} deriving (Show)

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

data TransformType = Discard | Irreversible | Reversible | Lossy deriving (Show, Enum)

data Transform = Transform {
    ttype    :: TransformType,
    declares :: [Blob],
    reverse  :: Maybe Trans
} deriving Show

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

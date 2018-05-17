module Entities (
    someFunc,
    getEntity,
    DRTEntity (..),
    Medium,
    Data (iblobId),
    Blob,
    Func,
    Transform (ReverseTransform),
    Trans (out, args),
    Arg(BlobArg),
    BlobID
) where

import           Control.Monad
import           Data.Binary.Get
import           Data.ByteString.Char8 (unpack)
import           Data.Word


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ID = Int
type BlobID = ID
type DataID = ID
type MediumID = ID
type FuncID = ID
type Tag = String
type Checksum = Int

data EntityType = BlobEntity | FuncEntity | DataEntity | TransformEntity | MediumEntity deriving (Enum, Show)

data DRTEntity = DRTBlob Blob | DRTFunc Func | DRTData Data | DRTTransform Transform | DRTMedium Medium deriving (Show)

getEntity :: Get DRTEntity
getEntity = do
        entity <- toEnum . fromIntegral <$> getWord8
        case entity of
            BlobEntity      -> DRTBlob <$> decodeBlob
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
    checksum :: Checksum,
    dataTags :: [Tag]
} deriving (Show)

decodeData =  liftM4 Data getDrtId getDrtId (fromIntegral <$> getWord32be) decodeTags

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
    return Trans {args = a, func = f, out = o}

data TransformType = DiscardTType | IrreversibleTType | ReversibleTType | LossyTType deriving (Enum)

data Transform = DiscardTransform | ForwardTransform Trans | ReverseTransform Trans deriving (Show)

decodeTransform = do
    t <- getWord8
    case toEnum (fromIntegral t) of
        DiscardTType      -> return DiscardTransform
        IrreversibleTType -> ForwardTransform <$> decodeTrans
        ReversibleTType   -> ReverseTransform <$> decodeTrans
        LossyTType        -> ReverseTransform <$> decodeTrans

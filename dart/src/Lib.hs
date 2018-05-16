module Entities (
    decodeMedium,
    decodeData,
    decodeBlob,
    decodeFunc,
    decodeTransform
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

data EntityType = BlobEntity | FuncEntity | DataEntity | TransformEntity | MediumEntity deriving (Enum)

class DRTEntity a where
    decode :: Get a

getDrtId :: Get ID
getDrtId = fromIntegral <$> getWord64be

getDrtLen :: Get Int
getDrtLen = fromIntegral <$> getWord64be

getDrtOffset :: Get Int
getDrtOffset = fromIntegral <$> getWord64be

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

decodeMedium :: Get Medium
decodeMedium = do
    i <- getDrtId
    t <- decodeTags
    return Medium {mediumId = i, mediumTags = t}

data Data = Data {
    dataId   :: DataID,
    checksum :: Checksum,
    dataTags :: [Tag]
} deriving (Show)

decodeData :: Get Data
decodeData = do
    d <- getDrtId
    c <- getWord32be
    t <- decodeTags
    return Data {dataId = d, checksum = fromIntegral c, dataTags = t}

data Blob = Blob {
    blobId     :: BlobID,
    parentData :: DataID,
    medium     :: MediumID,
    offset     :: Int,
    blobLength :: Int
} deriving (Show)

decodeBlob :: Get Blob
decodeBlob = do
    i <- getDrtId
    d <- getDrtId
    m <- getDrtId
    o <- getDrtOffset
    l <- getDrtOffset
    return Blob {blobId = i, parentData = d, medium = m, offset = o, blobLength = l}

data Func = Func {
    funcId :: FuncID,
    code   :: String
} deriving (Show)

decodeFunc :: Get Func
decodeFunc = do
    i <- getDrtId
    l <- getDrtLen
    b <- getByteString l
    return Func {funcId = i, code = unpack b}

data ArgType = BlobArgType | InlineArgType deriving (Enum)
data Arg = BlobArg BlobID | InlineArg String

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

decodeTransform :: Get Transform
decodeTransform = do
    t <- getWord8
    case toEnum (fromIntegral t) of
        DiscardTType      -> return DiscardTransform
        IrreversibleTType -> ForwardTransform <$> decodeTrans
        ReversibleTType   -> ReverseTransform <$> decodeTrans
        LossyTType        -> ReverseTransform <$> decodeTrans

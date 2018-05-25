module Entities where

import           Data.Word

type ID = Int
type BlobID = ID
type DataID = ID
type MediumID = ID
type FuncID = ID
type Tag = String
type Checksum = Word32

data Medium = Medium {
    mediumId   :: MediumID,
    mediumTags :: [Tag]
} deriving (Show)

data Data = Data {
    dataId   :: DataID,
    iblobId  :: BlobID, -- This is for first implicit data blob
    size     :: Int,
    checksum :: Checksum,
    dataTags :: [Tag]
} deriving (Show)

data Blob = Blob {
    blobId     :: BlobID,
    medium     :: MediumID,
    offset     :: Int,
    blobLength :: Int
} deriving (Show)

data Func = Func {
    funcId :: FuncID,
    code   :: String
} deriving (Show)

data Arg = BlobArg BlobID | InlineArg String deriving (Show)

data Trans = Trans {
    args :: [Arg],
    func :: FuncID,
    out  :: [BlobID]
} deriving (Show)

data TransformType = Discard | Irreversible | Reversible | Lossy deriving (Show, Enum)

data Transform = Transform {
    ttype    :: TransformType,
    declares :: [Blob],
    reverse  :: Maybe Trans
} deriving Show

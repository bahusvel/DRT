module Tree (
    logToTree,
    DRTT (dataTrees, blobs),
    TreeD (TreeD, drtData, transforms),
    TreeB (source, drtBlob),
    TreeT (inBlobs, drtTransform)
) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Entities
import           Log

data DRTT = DRTT {
    dataTrees :: [TreeD],
    mediums   :: [Medium],
    funcs     :: [Func],
    blobs     :: [Blob]
} deriving (Show)

data TreeD = TreeD {
    drtData    :: Data,
    transforms :: [TreeT]
} deriving (Show)

data TreeB = TreeB {
    drtBlob :: BlobID,
    source  :: [TreeT] -- These are potential sources
} deriving (Show)

data TreeT = TreeT {
    drtTransform :: Trans,
    inBlobs      :: [TreeB] -- These are required argument blobs
} deriving (Show)

genTreeT :: [Trans] -> BlobID -> [TreeT]
genTreeT l b = map (\t -> TreeT t (genTreeB l t)) source_ts
    where
        source_ts = filter (elem b . out) l -- transactions that output b


genTreeB :: [Trans] -> Trans -> [TreeB]
genTreeB l t = map (\b -> TreeB b (genTreeT l b)) blob_args
    where
        blob_args = [x | BlobArg x <- args t]

genTreeD :: DRTLog -> Data -> TreeD
genTreeD l d = TreeD d (genTreeT reversible (iblobId d))
    where
        reversible = [x | (DRTTransform (Transform _ _ (Just x))) <- l]

logToTree :: DRTLog -> DRTT
logToTree l = DRTT t [x | DRTMedium x <- l] [x | DRTFunc x <- l] t_blobs
    where
        t = map (genTreeD l) [x | DRTData x <- l]
        t_blobs = concat [d | DRTTransform (Transform _ d _) <- l]

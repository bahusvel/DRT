module Tree (
    logToTree,
    DRTT (dataTrees),
    TreeD (TreeD),
) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Entities

data DRTT = DRTT {
    dataTrees :: [TreeD],
    mediums   :: [Medium],
    funcs     :: [Func]
} deriving (Show)

data TreeD = TreeD {
    drtData    :: Data,
    transforms :: [TreeT]
} deriving (Show)

data TreeB = TreeB {
    drtBlob :: BlobID,
    source  :: [TreeT]
} deriving (Show)

data TreeT = TreeT {
    drtTransform :: Trans,
    inBlobs      :: [TreeB]
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
logToTree l = DRTT t [x | DRTMedium x <- l] [x | DRTFunc x <- l]
    where
        t = map (genTreeD l) [x | DRTData x <- l]

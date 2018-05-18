{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List

import           Entities

type DRTLog = [DRTEntity]

decodeLog :: BL.ByteString -> DRTLog
decodeLog b | BL.null b = []
decodeLog b = case runGetOrFail getEntity b of
        Left  (_, _, _)        -> []
        Right (leftover, _, a) -> a : decodeLog leftover

data DRTT = DRTT {
    dataTrees :: [TreeD],
    medium    :: [Medium],
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
        reversible = [x | (DRTTransform (ReverseTransform x)) <- l]

logToTree :: DRTLog -> DRTT
logToTree l = DRTT t [x | DRTMedium x <- l] [x | DRTFunc x <- l]
    where
        t = map (genTreeD l) [x | DRTData x <- l]

main :: IO ()
main = do
    f <- BL.readFile "../test/test.drtl"
    let l = decodeLog f
    let t = logToTree l
    print l
    print t
    return ()

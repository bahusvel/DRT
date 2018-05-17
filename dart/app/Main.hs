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
    dataTrees :: [DataTree],
    medium    :: [Medium],
    funcs     :: [Func]
}

data DataTree = DataTree {
    drtData    :: Data,
    transforms :: [TreeT]
}

data TreeB = TreeB {
    drtBlob :: BlobID,
    source  :: [TreeT]
}

data TreeT = TreeT {
    drtTransform :: Trans,
    inBlobs      :: [TreeB]
}

genTreeB :: [Trans] -> BlobID -> TreeB
genTreeB l b = TreeB b source_tts
    where
        source_ts = filter (elem b . out) l
        source_tts = map (\t -> TreeT t  ))  source_ts


genTreeT :: [Trans] -> Trans -> TreeT
genTreeT l t = TreeT t treebs
    where
        blob_args = filter (\case {BlobArg _ -> True; _ -> False}) $ map args source_t
        treebs = map (genTreeB ts) blob_args

genDataTree :: DRTLog -> Data -> DataTree
genDataTree l d = DataTree d source_tts
    where
        ts = map (\(DRTTransform b) -> b) $ filter (\case {DRTTransform (ReverseTransform _) -> True; _ -> False}) l
        source_ts = filter (elem (iblobId d) . out) ts
        source_tts = map (\t -> TreeT t (genTreeT t)) source_ts

logToTree :: DRTLog -> DRTT
logToTree l = DRTT t m f
    where   t = []
            m = map (\(DRTMedium b) -> b) $ filter (\case {DRTMedium _ -> True; _ -> False}) l
            f = map (\(DRTFunc b) -> b) $ filter (\case {DRTFunc _ -> True; _ -> False}) l

main :: IO ()
main = do
    f <- BL.readFile "../test/test.drtl"
    print $ decodeLog f
    return ()

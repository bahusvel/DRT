{-# LANGUAGE LambdaCase #-}
module Recovery (
    recoverData,
    RecoveryContext(RecoveryContext),
    RecoveryBlob(blobData),
    RecArg,
    splitOn
) where

import qualified Data.ByteString.Lazy as BL
import           Data.Digest.CRC32
import           Data.List
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Entities
import qualified Entities             as E
import           Funcs
import           System.Directory
import           Tree

type RecoveryMedium = BL.ByteString

data RecoveryContext = RecoveryContext {
    mediums         :: Map.Map MediumID RecoveryMedium,
    funcs           :: Map.Map FuncID RecFunc,
    drtt            :: DRTT,
    verifyIntegrity :: Bool
}

data RecoveryBlob = RecoveryBlob {
    recoveryBlobId :: BlobID,
    blobData       :: BL.ByteString
} deriving (Eq, Show)

bsRange :: BL.ByteString -> Int -> Int -> BL.ByteString
bsRange bs l h = BL.drop (fromIntegral l) $ BL.take (fromIntegral h) bs

processBlob :: RecoveryContext -> TreeB -> Maybe RecoveryBlob
processBlob c t = do
    b <- find (\b -> blobId b == drtBlob t) $ blobs $ drtt c
    let m = mediums c Map.!? medium b
    case m of
        Nothing -> listToMaybe $ mapMaybe (\st -> processTransform c st (blobId b)) $ source t
        Just rm -> Just $ RecoveryBlob (blobId b) (bsRange rm (offset b) (offset b + blobLength b))

processFunc :: RecoveryContext -> Trans -> [RecoveryBlob] -> [BL.ByteString]
processFunc c t bs = do
    let trans_args = E.args t
    let real_args = map (\case
            BlobArg bid -> RecArgBlob $ blobData $ fromJust $ find (\b -> recoveryBlobId b == bid) bs
            InlineArg iarg -> RecArgInline iarg
            ) trans_args
    let f = fromJust $ funcs c Map.!? func t
    f real_args

processTransform :: RecoveryContext -> TreeT -> BlobID -> Maybe RecoveryBlob
processTransform c t bi = do
    let blob_args = map (processBlob c) $ inBlobs t
    if Nothing `elem` blob_args
        then Nothing
        else do
            let just_args = catMaybes blob_args
            let out = processFunc c (drtTransform t) just_args
            blob_index <- elemIndex bi $ E.out $ drtTransform t
            return $ RecoveryBlob bi (out !! blob_index)

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn d l = (x, y)
    where
        i = elemIndex d l
        x = take (fromMaybe 0 i) l
        y = drop (maybe 0 (1 +) i) l

nameForData :: Data -> String
nameForData d = fromMaybe (show (dataId d) ++ ".data") ("name" `lookup` split )
    where
        split = map (splitOn ':') $ dataTags d

validateDataBlob :: Data -> RecoveryBlob -> Bool
validateDataBlob d b = crc32 (blobData b) == checksum d

recoverData :: RecoveryContext -> DataID -> IO ()
recoverData c d = do
    let dt = dataTrees (drtt c) !! d
    let dataBlobId = iblobId $ drtData dt
    createDirectoryIfMissing True "./data"
    let rcblobs = map (\d -> processTransform c d dataBlobId) $ transforms dt
    let good_blobs = filter (validateDataBlob (drtData dt)) $ catMaybes rcblobs
    case good_blobs of
        []    -> putStrLn $ "Recovery of " ++ show d ++ " failed."
        (b:_) -> do
            BL.writeFile ("./data/" ++ nameForData (drtData dt)) $ blobData b
            putStrLn $ "Recovery of " ++ show d ++ " succesful."
    return ()

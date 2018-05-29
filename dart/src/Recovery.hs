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
import           Data.Either
import           Data.List
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Entities
import qualified Entities             as E
import           Funcs
import           System.Directory
import           Text.Printf
import           Tree

type RecoveryMedium = BL.ByteString

data RecoveryContext = RecoveryContext {
    availableBlobs  :: Map.Map BlobID BL.ByteString,
    funcs           :: Map.Map FuncID RecFunc,
    drtt            :: DRTT,
    verifyIntegrity :: Bool
}

data RecoveryBlob = RecoveryBlob {
    recoveryBlobId :: BlobID,
    blobData       :: BL.ByteString
} deriving (Eq, Show)

processBlob :: RecoveryContext -> TreeB -> Either String RecoveryBlob
processBlob ctx t = do
    let maybeBlob = availableBlobs ctx Map.!? drtBlob t
    case maybeBlob of
        Just b -> Right $ RecoveryBlob (drtBlob t) b
        Nothing -> do
            let srcs = map (\st -> processTransform ctx st (drtBlob t)) $ source t
            case partitionEithers srcs of
                ([], b:_) -> Right b
                (errors, _) -> Left $ printf "Blob %s was not found and could not be derived because %s" (show (drtBlob t)) (unlines $ map ("\t" ++) errors)

processFunc :: RecoveryContext -> Trans -> [RecoveryBlob] -> [BL.ByteString]
processFunc c t bs = do
    let trans_args = E.args t
    let real_args = map (\case
            BlobArg bid -> RecArgBlob $ blobData $ fromJust $ find (\b -> recoveryBlobId b == bid) bs
            InlineArg iarg -> RecArgInline iarg
            ) trans_args
    let f = fromJust $ funcs c Map.!? func t
    f real_args

processTransform :: RecoveryContext -> TreeT -> BlobID -> Either String RecoveryBlob
processTransform c t bi = do
    let blob_args = map (processBlob c) $ inBlobs t
    case partitionEithers blob_args of
        ([], blobs) -> do
            let out = processFunc c (drtTransform t) blobs
            let blob_index = fromJust $ elemIndex bi $ E.out $ drtTransform t
            return $ RecoveryBlob bi (out !! blob_index)
        (errors, _) -> Left $ printf "Cannot evaluate transform producing blob %s because %s which are its arguments" (show bi) (unlines $ map ("\t" ++) errors)

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
recoverData ctx d = do
    let dt = dataTrees (drtt ctx) !! d
    let dataBlobId = iblobId $ drtData dt
    createDirectoryIfMissing True "./data"
    let recBlobs =  map (\d -> processTransform ctx d dataBlobId) $ transforms dt

    let good_blobs = if verifyIntegrity ctx
        then map (\b -> do
            rb <- b
            if validateDataBlob (drtData dt) rb
                then Right rb
                else Left "Data blob is corrupt"
                )  recBlobs
        else recBlobs

    case partitionEithers good_blobs of
        (errors, [])  -> putStrLn $ "Recovery of " ++ show d ++ " failed attempted:\n" ++ unlines (zipWith (printf "\tAttempt %d - %s") ([0..] :: [Int]) errors)
        (_, b:_) -> do
            BL.writeFile ("./data/" ++ nameForData (drtData dt)) $ blobData b
            putStrLn $ "Recovery of " ++ show d ++ " succesful."
    return ()

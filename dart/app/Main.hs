{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy   as BL
import           Data.List
import           Data.Maybe
import           System.Console.CmdArgs
import qualified System.Console.CmdArgs as CA

import           Entities
import           Tree

decodeLog :: BL.ByteString -> DRTLog
decodeLog b | BL.null b = []
decodeLog b = case runGetOrFail getEntity b of
        Left  (_, _, _)        -> []
        Right (leftover, _, a) -> a : decodeLog leftover

data Dart = List {
    drt :: FilePath
} | Recover {
    drt             :: FilePath,
    ids             :: [Int],
    recoveryMediums :: [(Int, FilePath)]
} deriving (Show, CA.Data, Typeable)

list = List def
recover = Recover def def def

loadAndConvert :: FilePath -> IO DRTT
loadAndConvert path = do
    f <- BL.readFile path
    let l = decodeLog f
    return $ logToTree l

doList :: Dart -> IO ()
doList (List file) = do
    t <- loadAndConvert file
    putStrLn "Datasets available in DRT file:"
    putStrLn "ID\tTags"
    mapM_ (\(TreeD d _) -> do
            let did = show (dataId d)
            let tags = intercalate "," (dataTags d)
            putStrLn (did ++ "\t" ++ tags)
        ) $ dataTrees t

type RecoveryMedium = BL.ByteString

data RecArg = RecArgBlob RecoveryBlob | RecArgInline String
type RecFunc = [RecArg] -> [RecoveryBlob]

data RecoveryContext = RecoveryContext {
    mediums :: [(MediumID, RecoveryMedium)],
    funcs   :: [(FuncID, RecFunc)],
    drtt    :: DRTT
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
    let m = find (\(m, _) -> m == medium b) $ mediums c
    case m of
        Nothing -> listToMaybe $ catMaybes $ map (\st -> processTransform c st (blobId b)) $ source t
        Just (_, rm) -> Just $ RecoveryBlob (blobId b) (bsRange rm (offset b) (offset b + blobLength b))

processTransform :: RecoveryContext -> TreeT -> BlobID -> Maybe RecoveryBlob
processTransform c t bi = do
    let blob_args = map (processBlob c) $ inBlobs t
    if elem Nothing args
        then Nothing
        else do
            let trans_fid = func $ drtTransform t
            let trans_args = args $ drtTransform t
            let real_args = map (\ta -> case ta of
                    BlobArg bid -> RecArgBlob $ fromJust $ find (\b -> blobId b == bid) blob_args
                    InlineArg iarg -> RecArgInline iarg
                    ) trans_args
            let (_, func) = fromJust $ find (\(fid, _) -> fid == trans_fid) $ funcs c
            let out = func real_args
            find (\b -> recoveryBlobId b == bi) out



recoverData :: RecoveryContext -> DataID -> IO ()
recoverData c d = do
    let dt = (dataTrees (drtt c)) !! d
    let dataBlobId = iblobId $ drtData dt
    let rcblobs = map (\d -> processTransform c d dataBlobId) $ transforms dt
    print rcblobs
    return ()

doRecover :: Dart -> IO ()
doRecover (Recover file ids mds) = do
    t <- loadAndConvert file
    ms <- sequence $ map (\(i, p) ->  liftM2 (,) (return i) (BL.readFile p)) mds

    let context = RecoveryContext ms t
    mapM_ (recoverData context) ids

main :: IO ()
main = do
    l <- cmdArgs (modes[list, recover])
    case l of
        (List _) -> doList l

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy   as BL
import           Data.List
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           System.Console.CmdArgs
import qualified System.Console.CmdArgs as CA
import           System.IO

import           Entities
import qualified Tree                   as T

import           Funcs
import           Log
import           Recovery
import           Tree

data ListModes = Datas | Blobs | Mediums deriving (CA.Data, Show)

data Dart = List {
    drt    :: FilePath,
    entity :: ListModes
} | Recover {
    drt      :: FilePath,
    ids      :: [Int],
    mediums  :: [(Int, FilePath)],
    noVerify :: Bool
} deriving (Show, CA.Data, Typeable)

list = List def $ enum [Datas &= explicit &= name "data" , Blobs &= explicit &= name "blobs", Mediums &= explicit &= name "mediums"]
recover = Recover def def def def

loadAndConvert :: FilePath -> IO DRTT
loadAndConvert path = do
    f <- BL.readFile path
    let l = decodeLog f
    return $ logToTree l

doList :: Dart -> IO ()
doList (List file entity) = do
    t <- loadAndConvert file
    case entity of
        Datas -> do
            putStrLn "Datasets available in DRT file:"
            putStrLn "ID\tTags"
            mapM_ (\(TreeD d _) -> do
                let tags = intercalate "," (dataTags d)
                putStrLn (show (dataId d) ++ "\t" ++ tags)
                ) $ dataTrees t
        Blobs -> do
            putStrLn "Blobs available in DRT file:"
            putStrLn "ID\tBlob"
            mapM_ (\b -> putStrLn (show (blobId b) ++ "\t" ++ show b)) $ blobs t
        Mediums -> do
            putStrLn "Mediums available in DRT file:"
            putStrLn "ID\tTags"
            mapM_ (\m -> do
                let tags = intercalate "," (mediumTags m)
                putStrLn (show (mediumId m) ++ "\t" ++ tags)
                ) $ T.mediums t


getBlobData :: Blob -> [(Int, Handle)] -> IO BL.ByteString
getBlobData b mh = do
    let h = fromJust $ medium b `lookup` mh
    hSeek h AbsoluteSeek (fromIntegral (offset b))
    BL.hGet h (blobLength b)

doRecover :: Dart -> IO ()
doRecover (Recover file ids mds dv) = do
    t <- loadAndConvert file
    let mids = [m | (m, _) <- mds]
    mhandles <- mapM (\(i, p) ->  liftM2 (,) (return i) (openFile p ReadMode)) mds
    let bs = filter (\b -> medium b `elem` mids) $ blobs t
    availBlobs <- mapM (\b -> liftM2 (,) (return (blobId b)) (getBlobData b mhandles)) bs
    let context = RecoveryContext (Map.fromList availBlobs) (Map.fromList funcTable) t (not dv)
    mapM_ (recoverData context) ids

main :: IO ()
main = do
    l <- cmdArgs (modes[list, recover])
    case l of
        List {}    -> doList l
        Recover {} -> doRecover l

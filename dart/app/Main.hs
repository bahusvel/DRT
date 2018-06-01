{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy   as BL
import           Data.Int
import           Data.List
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           System.Console.CmdArgs
import qualified System.Console.CmdArgs as CA
import           System.IO
import           Text.Regex.TDFA


import           Entities
import qualified Tree                   as T

import           Funcs
import           Log
import           Recovery
import           Tree

data ListModes = Datas | Blobs | Mediums deriving (CA.Data, Show)

data Dart = List {
    drt         :: FilePath,
    queryFilter :: [(String, String)],
    entity      :: ListModes
} | Recover {
    drt         :: FilePath,
    queryFilter :: [(String, String)],
    ids         :: [Int64],
    mediums     :: [(Int64, FilePath)],
    noVerify    :: Bool
} deriving (Show, CA.Data, Typeable)

list = List def (def &= explicit &= name "filter") $ enum [Datas &= explicit &= name "data" , Blobs &= explicit &= name "blobs", Mediums &= explicit &= name "mediums"]

recover = Recover def (def &= explicit &= name "filter") def def def

loadAndConvert :: FilePath -> IO DRTT
loadAndConvert path = do
    f <- BL.readFile path
    let l = decodeLog f
    return $ logToTree l

doList :: Dart -> IO ()
doList (List file filt entity) = do
    t <- loadAndConvert file
    case entity of
        Datas -> do
            putStrLn "Datasets available in DRT file:"
            putStrLn "ID\tTags"
            let ds = map drtData (dataTrees t)
            let ds' = if null filt then ds else filter (filterOnTags filt . dataTags) ds
            mapM_ (\d -> do
                let tags = intercalate "," (dataTags d)
                putStrLn (show (dataId d) ++ "\t" ++ tags)
                ) ds'
        Blobs -> do
            putStrLn "Blobs available in DRT file:"
            putStrLn "ID\tBlob"
            mapM_ (\b -> putStrLn (show (blobId b) ++ "\t" ++ show b)) $ blobs t
        Mediums -> do
            putStrLn "Mediums available in DRT file:"
            putStrLn "ID\tTags"
            let ms = if null filt
                then T.mediums t
                else filter (filterOnTags filt . mediumTags) $ T.mediums t
            mapM_ (\m -> do
                let tags = intercalate "," (mediumTags m)
                putStrLn (show (mediumId m) ++ "\t" ++ tags)
                ) ms


getBlobData :: Blob -> [(Int64, Handle)] -> IO BL.ByteString
getBlobData b mh = do
    let h = fromJust $ medium b `lookup` mh
    hSeek h AbsoluteSeek (fromIntegral (offset b))
    BL.hGet h $ fromIntegral $ blobLength b

filterOnTags ::  [(String, String)] -> [Tag] -> Bool
filterOnTags filters ts =  False `notElem` filter_matches
    where
        split_tags = map (splitOn ':') ts
        filter_matches = map (\(t, v) -> fromMaybe False (do
                tag_value <- t `lookup` split_tags
                return (tag_value =~ v :: Bool)
            )) filters


doRecover :: Dart -> IO ()
doRecover (Recover file filt ids mds dv) = do
    t <- loadAndConvert file
    let ids' = if null ids
        then [ dataId (drtData td) | td <- dataTrees t]
        else ids

    let ids'' = if null filt
        then ids'
        else map (dataId) $ filter (filterOnTags filt . dataTags) (
            filter (\d -> dataId d `elem` ids) $ map drtData (dataTrees t)
            )

    let mids = [m | (m, _) <- mds]
    mhandles <- mapM (\(i, p) ->  liftM2 (,) (return i) (openFile p ReadMode)) mds

    let bs = filter (\b -> medium b `elem` mids) $ blobs t
    availBlobs <- mapM (\b -> liftM2 (,) (return (blobId b)) (getBlobData b mhandles)) bs

    let context = RecoveryContext (Map.fromList availBlobs) (Map.fromList funcTable) t (not dv)

    mapM_ (recoverData context . fromIntegral) ids''

main :: IO ()
main = do
    l <- cmdArgs (modes[list, recover])
    case l of
        List {}    -> doList l
        Recover {} -> doRecover l

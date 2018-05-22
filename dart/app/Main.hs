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

import           Entities

import           Funcs
import           Log
import           Recovery
import           Tree

data Dart = List {
    drt :: FilePath
} | Recover {
    drt     :: FilePath,
    ids     :: [Int],
    mediums :: [(Int, FilePath)]
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


doRecover :: Dart -> IO ()
doRecover (Recover file ids mds) = do
    t <- loadAndConvert file
    ms <- mapM (\(i, p) ->  liftM2 (,) (return i) (BL.readFile p)) mds
    let context = RecoveryContext (Map.fromList ms) (Map.fromList funcTable) t
    mapM_ (recoverData context) ids

main :: IO ()
main = do
    l <- cmdArgs (modes[list, recover])
    case l of
        List {}    -> doList l
        Recover {} -> doRecover l

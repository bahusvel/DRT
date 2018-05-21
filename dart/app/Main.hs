{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy   as BL
import           Data.List
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
    drt :: FilePath,
    ids :: [Int]
} deriving (Show, CA.Data, Typeable)

list = List def
recover = Recover def def

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
doList _ = fail "Not possible"

recoverData :: DRTT -> DataID -> IO ()
recoverData t d = return ()

doRecover :: Dart -> IO ()
doRecover (Recover file ids) = do
    t <- loadAndConvert file
    mapM_ (recoverData t) ids

main :: IO ()
main = do
    l <- cmdArgs (modes[list, recover])
    case l of
        (List _) -> doList l

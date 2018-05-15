module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ID = Int
type BlobID = ID
type DataID = ID
type MediumID = ID

data Blob = Blob {
    id :: BlobID,
    data :: DataID,
    medium :: MediumID,
    offset :: Int,
    length :: Int
}

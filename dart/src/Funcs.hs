module Funcs (
    funcTable,
    RecFunc,
    RecArg(..),
) where

import qualified Data.ByteString.Lazy as BL
type FuncID = Int

type FuncBlob = BL.ByteString
data RecArg = RecArgBlob FuncBlob | RecArgInline String
type RecFunc = [RecArg] -> [FuncBlob]

funcTable :: [(FuncID, RecFunc)]
funcTable = [(1, merge)]

merge :: [RecArg] -> [FuncBlob]
merge args = [BL.concat [x | RecArgBlob x <- args]]

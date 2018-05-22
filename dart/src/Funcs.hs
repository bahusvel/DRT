module Funcs (
    funcTable,
    RecFunc,
    RecArg(..),
) where

import qualified Data.ByteString.Lazy as BL
import           Entities             (FuncID)

type FuncBlob = BL.ByteString
data RecArg = RecArgBlob FuncBlob | RecArgInline String
type RecFunc = [RecArg] -> [FuncBlob]

funcTable :: [(FuncID, RecFunc)]
funcTable = [(1, merge)]

merge :: [RecArg] -> [FuncBlob]
merge args = [BL.concat [x | RecArgBlob x <- args]]

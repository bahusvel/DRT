module UserFuncs (

) where

import Data.Either
import Control.Monad
import Language.Haskell.Interpreter
import Funcs

userFunc =  "(\\args -> [BL.reverse $ head [x | RecArgBlob x <- args]])"

testUserFunc :: IO RecFunc
testUserFunc = do
    r <- runInterpreter (do
            loadModules ["src/Funcs.hs"]
            setImports ["Prelude"]
            setImportsQ [("Funcs", Nothing), ("Data.ByteString.Lazy", Just "BL"), ("Data.ByteString.Lazy", Nothing)]
            interpret userFunc (as :: RecFunc)
        )
    case r of
        Right r -> return r
        Left l -> fail $ show l

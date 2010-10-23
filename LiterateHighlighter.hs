module Main where

import Data.List

import System( getArgs )
import System.IO


import Language.Haskell.Exts

-----------------------------

import Literate.Haskell 
import Literate.SimpleInfo

-----------------------------


main = do args <- getArgs
          if length args < 2
            then
              putStrLn "Please provide two arguments exec INPUT OUTPUT"
            else
              do hSetEncoding stdin  utf8
                 hSetEncoding stdout utf8
                 hSetEncoding stderr utf8
                 input  <- parseFile (head args)
                 output <- openFile  (head $ tail args) WriteMode
                 let m = fromParse input
                 let si = simpleinfo{ types          = listTypes m
                                    , literalNumbers = listLitNumbers m
                                    , constructors   = listConstructors m
                                    , functions      = functionBindings m
                                    }
                 mapM_ (\(keyword, f) -> mapM_ (\ (seek,rep) -> hPutStrLn output $ printFormat keyword seek rep) 
                                         (f si)) 
                        mapping
  where printFormat keyword seek rep = "%format " ++ seek ++ " = \" {\\lhsCH" ++ keyword ++ "{" ++ rep ++ "}}\"" 

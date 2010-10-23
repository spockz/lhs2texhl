module Main where

import Data.List

import System( getArgs )
import System.IO


import Language.Haskell.Exts

-----------------------------

import Literate.Agda
import Literate.Haskell 
import Literate.SimpleInfo

-----------------------------


main = do args <- getArgs
          if length args < 2
            then
              putStrLn "Please provide two arguments exec INPUT OUTPUT"
            else
              do 
                 hSetEncoding stdin  utf8
                 hSetEncoding stdout utf8
                 hSetEncoding stderr utf8
                 
                 output <- openFile  (head $ tail args) WriteMode
                 hSetEncoding output utf8

                 let writer = writeOutput output
                                  
                 if (elem "--agda" args)
                   then
                     do si <- runAgda (head args)
                        writer si Literate.Agda.mapping
                   else
                     do si <- runHaskell (head args)
                        writer si Literate.Haskell.mapping
                 hClose output
  where printFormat keyword seek rep = "%format " ++ seek ++ " = \" {\\lhsCH" ++ keyword ++ "{" ++ rep ++ "}}\"" 
        writeOutput output si mapping = 
                      mapM_ (\(keyword, f) -> mapM_ (\ (seek,rep) -> hPutStrLn output $ printFormat keyword seek rep) 
                                              (f si)) 
                                              mapping


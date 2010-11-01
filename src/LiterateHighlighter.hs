module Main where

import Data.List

import System( getArgs )
import System.IO


import Language.Haskell.Exts

-----------------------------

import Base.CLI
import System.Console.CmdArgs

import Literate.Agda
import Literate.Haskell 
import Literate.SimpleInfo

-----------------------------


main = do args <- cmdArgsRun standard

          hSetEncoding stdin  utf8
          hSetEncoding stdout utf8
          hSetEncoding stderr utf8
         
          hOutput <- openFile  (output args) WriteMode
          hSetEncoding hOutput utf8

          let writer = writeOutput hOutput
                           
          if (agda_mode args)
            then
              mapM_ (\file -> runAgda file
                     >>= (flip writer) Literate.Agda.mapping)
                    (input args)
            else
              mapM_ (\file -> runHaskell file
                     >>= (flip writer) Literate.Haskell.mapping)
                    (input args)
          
          hClose hOutput
  where printFormat keyword seek rep = "%format " ++ seek ++ " = \" {\\lhsCH" ++ keyword ++ "{" ++ rep ++ "}}\"" 
        writeOutput output si mapping = 
                      mapM_  (\(keyword, f) -> mapM_ (\ (seek,rep) -> hPutStrLn output $ printFormat keyword seek rep) 
                                                     (filter lhs2TeXSafe (f si))
                             ) 
                             mapping
                    
lhs2TeXSafe :: (String, String) -> Bool
lhs2TeXSafe ("()" , _)  = False
lhs2TeXSafe _           = True


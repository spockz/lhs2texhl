module Main where

import Data.List

import System.Environment( getArgs )
import System.IO


import Language.Haskell.Exts

-----------------------------

import Base.CLI
import System.Console.CmdArgs

-- import Literate.Agda
import Literate.Haskell 
import Literate.SimpleInfo


import Language.Markup
-----------------------------


main = do args <- cmdArgsRun standard

          hSetEncoding stdin  utf8
          hSetEncoding stdout utf8
          hSetEncoding stderr utf8
         
          case (action args) of
            ListCommands -> putStr newCommands
            _            -> if (agda_mode args)
                              then
                                error "Agda mode is currently not supported."
                              else 
                                printFormatting args

  where printFormat keyword seek rep = "%format " ++ seek ++ " = \" {\\lhsCH" ++ keyword ++ "{" ++ rep ++ "}}\"" 
        writeOutput output si mapping = 
                      mapM_  (\(keyword, f) -> mapM_ (\ (seek,rep) -> hPutStrLn output $ printFormat keyword seek rep) 
                                                     (filter lhs2TeXSafe (f si))
                             ) 
                             mapping
        printFormatting args = do  hOutput <- openFile (output args) WriteMode
                                   let writer = writeOutput hOutput
                                   hSetEncoding hOutput utf8
                                   mapM_ (\file -> runHaskell file
                                          >>= (flip writer) Literate.Haskell.mapping)
                                         (input args)
                                   hClose hOutput
        
                    
lhs2TeXSafe :: (String, String) -> Bool
lhs2TeXSafe ("()" , _)  = False
lhs2TeXSafe _           = True


{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Main where

import Data.List

import System.Environment( getArgs )
import System.IO

import Data.Maybe (fromJust)

import Language.Haskell.Exts

import Text.ParserCombinators.UU.Core
import qualified Text.ParserCombinators.UU.Core as PCC                         ( parse )
import Text.ParserCombinators.UU.BasicInstances hiding (input)
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils

import System.FilePath ( takeDirectory )
-----------------------------

import Base.CLI
import System.Console.CmdArgs

import Literate.Haskell 
import Literate.SimpleInfo


import Language.Markup


-----------------------------


main = do args <- cmdArgsRun standard
          goUTF8
         
          case action args of
            ListCommands -> putStr newCommands
            _            -> if agda_mode args
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
                                          >>= flip writer Literate.Haskell.mapping)
                                         (input args)
                                   hClose hOutput
        goUTF8 = do hSetEncoding stdin  utf8
                    hSetEncoding stdout utf8
                    hSetEncoding stderr utf8                                   

discoverFiles :: FilePath -> IO [FilePath]
discoverFiles fp = do contents  <- fmap (\xs -> [base ++ x | Just x <- map runPInclude (lines xs)])
                                        (readFile fp)
                      files <- mapM discoverFiles contents
                      return (nub $ fp : concat files)
  where files = undefined
        base = takeDirectory fp ++ "/"

  
runPInclude xs@('%':_) = runParse pInclude xs
runPInclude _          = Nothing

runParse :: Show t => Parser t -> String -> Maybe t
runParse p inp = let r@(a, errors) = PCC.parse (  (,) <$> p <*> pEnd) 
                                               (createStr (LineColPos 0 0 0) inp)
                 in if null errors then
                        Just a
                      else
                        Nothing
  
pInclude :: Parser FilePath
pInclude = (++ ".lhs") <$> (   pSymbol "%"
                           *>  pSymbol "include"
                           *>  pSome (pLetter <|> pDigit <|> pSym '/')
                           <*  pSymbol ".lhs")
        
isJust :: Maybe a -> Bool        
isJust (Nothing) = False
isJust (Just  _) = True
                    
lhs2TeXSafe :: (String, String) -> Bool
lhs2TeXSafe ("()" , _)  = False
lhs2TeXSafe _           = True


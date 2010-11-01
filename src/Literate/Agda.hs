{-# LANGUAGE NamedFieldPuns #-}
module Literate.Agda (runAgda, mapping) where

import Data.List (nub)
import Data.Data
import Data.Generics

---


import Agda.Syntax.Concrete
import Agda.Syntax.Literal

import Agda.Syntax.Parser
import Agda.Utils.FileName

----

import Language.LaTeX

import Literate.SimpleInfo

----

runAgda :: FilePath -> IO SimpleInfo
runAgda path = do  abpath <- absolute path
                   mod <- parseFile' moduleParser abpath
                   return simpleinfo{ functions = []
                                    , operators = listOperators mod
                                    , literalNumbers = listLiteralNumbers mod
                                    }

mapping :: [(String, SimpleInfo -> [(String,String)])]
mapping = [ 
            ("litNumber",     mnumbers)
          , ("infixoperator", moperators)
          ]
                                
listOperators :: Module -> [String]
listOperators =  nub . everything (++) ([] `mkQ` listOperator)
 where  listOperator :: Expr -> [String]
        listOperator (OpApp _ n _) = [show n]
        listOperator (_)  = []
        
listLiteralNumbers :: Module -> [String]
listLiteralNumbers =  nub . everything (++) ([] `mkQ` listLiteralNumber)
 where  listLiteralNumber :: Literal -> [String]
        listLiteralNumber (LitInt   _ v) = [show v]
        listLiteralNumber (LitFloat _ v) = [show v]
        listLiteralNumber (_)  = []
        
mnumbers SimpleInfo{literalNumbers} = map dp literalNumbers
moperators SimpleInfo{operators} = map (\ a -> (a, "\\ "++ makeLatexSafe a++"\\ ")) 
                                       operators

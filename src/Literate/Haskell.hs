{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Literate.Haskell (runHaskell, mapping, listClasses, fromParse) where

import Data.List (nub)
import Data.Data
import Data.Generics
import Language.Haskell.Exts

import Language.LaTeX
import Literate.SimpleInfo
 

newtype M = M Module deriving (Typeable, Data)


runHaskell :: FilePath -> IO SimpleInfo
runHaskell fp = do mod <- parseFileWithMode (defaultParseMode { fixities = baseFixities } ) 
                                            fp
                   case mod of
                     (ParseOk m)           -> return $ getSimpleInfo m
                     (ParseFailed loc err) -> error $ 
                                                "Parsing failed at `" 
                                                ++ show loc
                                                ++ " " ++ err


{- SYB Queries -}
listTypes :: Module -> [String]
listTypes m = (map prettyPrint (collectTypes m))
              where
  collectTypes :: Module -> [QName]
  collectTypes = nub . everything (++) ([] `mkQ` getData) where
    getData :: Type -> [QName]
    getData (TyCon n) = [n]
    getData _         = []

listConstructors ::  Module -> [String]
listConstructors =  nub . everything (++) ([] `mkQ` listConstructor)
 where  listConstructor :: ConDecl -> [String]
        listConstructor (ConDecl (i) _)      = [prettyPrint i]
        listConstructor (InfixConDecl _ i _) = [prettyPrint i]
        listConstructor (RecDecl i _)        = [prettyPrint i]

listFunctions ::  Module -> [String]
listFunctions =  nub . everything (++) ([] `mkQ` functionBinding `extQ` functionUse)
 where  functionBinding :: Match -> [String]
        functionBinding (Match _ (i) _ _ _ _)  = [prettyPrint i]
        functionUse :: Exp -> [String] 
        functionUse (App (Var qname) _) = [prettyPrint qname]
        functionUse _                   = []
        
listOperators ::  Module -> [String]
listOperators =  nub . everything (++) ([] `mkQ` operatorUse)
 where  operatorUse :: Exp -> [String] 
        operatorUse (InfixApp _ qop _)  = [prettyPrint qop]
        operatorUse _                   = []

listClasses ::  Module -> [String]
listClasses =  nub . everything (++) ([] `mkQ` listClassDeriving
                                         `extQ` listClassDecl
                                         `extQ` listClassContext)
 where  listClassDeriving :: Deriving -> [String] 
        listClassDeriving (name , _)  = [prettyPrint name]
        listClassDecl :: Decl -> [String]
        listClassDecl (ClassDecl _ _ name _ _ _) = [prettyPrint name]
        listClassDecl _ = []
        listClassContext :: Asst -> [String]
        listClassContext (ClassA name _) = [prettyPrint name]
        listClassContext _               = []


getSimpleInfo m = simpleinfo{ types          = listTypes m
                            , constructors   = listConstructors m
                            , functions      = listFunctions m
                            , operators      = listOperators m
                            , classes        = listClasses   m
                            }



mapping :: [(String, SimpleInfo -> [(String,String)])]
mapping = [ ("syntax",       syntax)
          , ("keyword",      keywords)
          , ("prelude",      prelude)
          , ("applicative",  applicative )
          , ("type",         mtypes) 
          , ("constructor",  mconstructors)
          , ("function",  mfunctions)
          , ("infixoperator", moperators)
          ]
          
mtypes :: SimpleInfo -> [(String, String)]
mtypes SimpleInfo{types} = map dp types
moperators SimpleInfo{operators} = map (\ a -> (a, "\\ "++ makeLatexSafe a++"\\ ")) 
                                       operators
mconstructors SimpleInfo{constructors} = map (dp) constructors
mfunctions SimpleInfo{functions   } = map (dp) functions

syntax _  = map dp  [ "=", "{", "}", "(", ")", "<-", "->", "=>", ","
                    ]

keywords _ = map dp [ "data", "deriving", "type", "instance", "family", "where"
                    , "newtype", "if", "then", "else", "case", "of", "module"
                    , "as", "hiding", "import", "let", "in", "do", "class"]

prelude  SimpleInfo{functions   } = map dp $
                    filter ((flip elem) functions)
                    ["abs" , "acos" , "acosh" , "all" , "and" , "any" , 
                     "appendFile" , "applyM" , "asTypeOf" , "asin" , "asinh" , 
                     "atan" , "atan2" , "atanh" , "break" , "catch" , "ceiling",
                     "compare" , "concat" , "concatMap" , "const" , "cos" , 
                     "cosh" , "curry" , "cycle" , "decodeFloat" , "div" , 
                     "divMod" , "drop" , "dropWhile" , "elem" , "encodeFloat" , 
                     "enumFrom" , "enumFromThen" , "enumFromThenTo" , 
                     "enumFromTo" , "error" , "even" , "exp" , "exponent" , 
                     "fail" , "filter" , "flip" , "floatDigits" , "floatRadix" , 
                     "floatRange" , "floor" , "fmap" , "foldl" , "foldl1" , 
                     "foldr" , "foldr1" , "fromEnum" , "fromInteger" , 
                     "fromIntegral" , "fromRational" , "fst" , "gcd" , 
                     "getChar" , "getContents" , "getLine" , "head" , "id" , 
                     "init" , "interact" , "ioError" , "isDenormalized" , 
                     "isIEEE" , "isInfinite" , "isNaN" , "isNegativeZero" , 
                     "iterate" , "last" , "lcm" , "length" , "lex" , "lines" , 
                     "log" , "logBase" , "lookup" , "map" , "mapM" , "mapM_" , 
                     "max" , "maxBound" , "maximum" , "maybe" , "min" , 
                     "minBound" , "minimum" , "mod" , "negate" , "not" , 
                     "notElem" , "null" , "odd" , "or" , "otherwise" , "pi" , 
                     "pred" , "print" , "product" , "properFraction" , 
                     "putChar" , "putStr" , "putStrLn" , "quot" , "quotRem" , 
                     "read" , "readFile" , "readIO" , "readList" , "readLn" , 
                     "readParen" , "reads" , "readsPrec" , "realToFrac" , 
                     "recip" , "rem" , "repeat" , "replicate" , "return" , 
                     "reverse" , "round" , "scaleFloat" , "scanl" , "scanl1" , 
                     "scanr" , "scanr1" , "seq" , "sequence" , "sequence_" , 
                     "show" , "showChar" , "showList" , "showParen" , 
                     "showString" , "shows" , "showsPrec" , "significand" , 
                     "signum" , "sin" , "sinh" , "snd" , "span" , "splitAt" , 
                     "sqrt" , "subtract" , "succ" , "sum" , "tail" , "take" , 
                     "takeWhile" , "tan" , "tanh" , "toEnum" , "toInteger" , 
                     "toRational" , "truncate" , "uncurry" , 
                     "unlines" , "until" , "unwords" , "unzip" , "unzip3" , 
                     "userError" , "words" , "writeFile" , "zip" , "zip3" , 
                     "zipWith" , "zipWith3", "$"]


applicative _ = []

fooz = [4, 13, 42]
douz = [4.0, 13.0, 42.0]



(<++>) :: a -> b -> a
(<++>) a b = a

tid :: Typeable a => a -> a
tid = id


fromParse (ParseOk m) = m



{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Literate.Haskell where

import Data.List (nub)
import Data.Data
import Data.Generics
import Data.String.Utils
import Language.Haskell.Exts

import Literate.SimpleInfo
 

newtype M = M Module deriving (Typeable, Data)



{- SYB Queries -}
listTypes :: Module -> [String]
listTypes m = (map prettyPrint (collectTypes m))
              where
  collectTypes :: Module -> [QName]
  collectTypes = nub . everything (++) ([] `mkQ` getData) where
    getData :: Type -> [QName]
    getData (TyCon n) = [n]
    getData _         = []


listLitNumbers ::  Module -> [String]
listLitNumbers =  nub . everything (++) ([] `mkQ` listLitNumber)
 where  listLitNumber :: Literal -> [String]
        listLitNumber (Char   _) = []
        listLitNumber (String _) = []
        listLitNumber (i)  = [prettyPrint i]


listConstructors ::  Module -> [String]
listConstructors =  nub . everything (++) ([] `mkQ` listConstructor)
 where  listConstructor :: ConDecl -> [String]
        listConstructor (ConDecl (i) _)  = [prettyPrint i]
        listConstructor (InfixConDecl _ i _) = [prettyPrint i]
        listConstructor (RecDecl i _)        = [prettyPrint i]

functionBindings ::  Module -> [String]
functionBindings =  nub . everything (++) ([] `mkQ` functionBinding `extQ` functionUse)
 where  functionBinding :: Match -> [String]
        functionBinding (Match _ (i) _ _ _ _)  = [prettyPrint i]
        functionUse :: Exp -> [String] 
        functionUse (App (Var qname) _) = [prettyPrint qname]
        functionUse _                   = []





mapping :: [(String, SimpleInfo -> [(String,String)])]
mapping = [ ("keyword",      keywords)
          , ("prelude",      prelude)
          , ("applicative",  applicative )
          , ("type",         mtypes) 
          , ("litNumber",    mnumbers)
          , ("constructor",  mconstructors)
          , ("function",  mfunctions)
          ]

keywords _ = map dp [ "data", "deriving", "type", "instance", "family", "where"
                    , "newtype", "if", "then", "else", "case", "of"]

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
                     "toRational" , "truncate" , "uncurry" , "undefined" , 
                     "unlines" , "until" , "unwords" , "unzip" , "unzip3" , 
                     "userError" , "words" , "writeFile" , "zip" , "zip3" , 
                     "zipWith" , "zipWith3"]


applicative _ = []

fooz = [4, 13, 42]
douz = [4.0, 13.0, 42.0]

mtypes :: SimpleInfo -> [(String, String)]
mtypes SimpleInfo{types} = map dp types
mnumbers SimpleInfo{literalNumbers} = map (dp.show) literalNumbers
mconstructors SimpleInfo{constructors} = map (dp) constructors
mfunctions SimpleInfo{functions   } = map (dp) functions

(<++>) :: a -> b -> a
(<++>) a b = a

dp :: String -> (String, String)
dp a = (a, (replace ">" "\\textgreater" . Data.String.Utils.replace "<" "\\textless") a)

fromParse (ParseOk m) = m
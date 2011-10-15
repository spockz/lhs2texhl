{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Literate.Haskell (runHaskell, mapping, fromParse) where

import Data.List (nub)
import Data.Maybe
import Data.Data
import Data.Generics
import Language.Haskell.Exts hiding (parseFile)

import Language.LaTeX
import Literate.SimpleInfo
 

newtype M = M Module deriving (Typeable, Data)

type ItemQuery a = a -> [Item]

newtype ConstructorSearch = ConstructorSearch Module deriving (Typeable, Show)
newtype FunctionSearch    = FunctionSearch    Module deriving (Typeable, Show)
newtype OperatorSearch    = OperatorSearch    Module deriving (Typeable, Show)
newtype ClassSearch       = ClassSearch       Module deriving (Typeable, Show)

parseFile fp = parseFileWithMode (defaultParseMode { fixities      = Just baseFixities
                                                   , parseFilename = fp      
                                                   } 
                                 )
                                 fp

runHaskell :: FilePath -> IO (Either String SimpleInfo)
runHaskell fp = do mod <- parseFile fp
                   case mod of
                     (ParseOk m)           -> (return . Right) (getSimpleInfo m)
                     (ParseFailed loc err) -> (return . Left) $ 
                                                "Parsing failed at `" 
                                                ++ show loc
                                                ++ " " ++ err
                                                

collect :: Module -> [Item]
collect   = nub . everything (++) ([] `mkQ`  searchTypes
                                      `extQ` searchConDecl
                                      `extQ` searchPat
                                      `extQ` searchExp
                                      `extQ` searchMat
                                      `extQ` searchDecl
                                      `extQ` searchDeriving
                                      `extQ` searchAsst)
  


{- SYB Queries -}
searchTypes :: ItemQuery Type
searchTypes (TyCon n) = [Type (prettyPrint n)]
searchTypes _         = []

searchConDecl :: ItemQuery ConDecl
searchConDecl (ConDecl (i) _)      = [Constructor $ prettyPrint i]
searchConDecl (InfixConDecl _ i _) = [Constructor $ prettyPrint i]
searchConDecl (RecDecl i _)        = [Constructor $ prettyPrint i]

searchPat :: ItemQuery Pat
searchPat (PApp i _)        = [Constructor $ prettyPrint i]
searchPat _                 = []

searchExp :: ItemQuery Exp
searchExp (Con i)             = [Constructor $ prettyPrint i]
searchExp (App (Var qname) _) = [Function $ prettyPrint qname]
searchExp (InfixApp _ qop _)  = [Operator $ prettyPrint qop]
searchExp _                   = []

searchMat :: ItemQuery Match
searchMat (Match _ (i) _ _ _ _)  = [Function $ prettyPrint i]

searchDecl :: ItemQuery Decl
searchDecl (TypeSig _ names t)  = case t of
                                    TyParen (TyFun _ _)  -> namesToFunction names
                                    TyFun   _    _       -> namesToFunction names
                                    TyForall _ _  t     -> case t of 
                                                             TyParen (TyFun _ _) -> namesToFunction names
                                                             TyFun _ _           -> namesToFunction names
                                                             _                   -> namesToConstant names
                                    _                    -> namesToConstant names
  where  nameToString :: (String -> Item) -> Name -> Item
         nameToString f (Ident s)  = f s
         nameToString f (Symbol s) = f s
         namesToFunction = map (nameToString Function)
         namesToConstant = map (nameToString Constant) 
         
         
         
         
         
searchDecl (ClassDecl _ _ name _ _ _) = [Class $ prettyPrint name]
searchDecl _ = []

        


searchDeriving :: ItemQuery Deriving
searchDeriving (name , _)  = [Class $ prettyPrint name]

searchAsst :: ItemQuery Asst
searchAsst (ClassA name _) = [Class $ prettyPrint name]
searchAsst _               = []


getSimpleInfo m = simpleinfo{ types          = f isT
                            , constructors   = f isCo
                            , functions      = f isF
                            , operators      = f isO
                            , classes        = f isCl
                            , constants      = f isConst
                            }
  where f p = map show (filter p collection)
        isT  (Type _) = True
        isT  _        = False
        isCo (Constructor _) = True
        isCo _               = False
        isF  (Function _)    = True
        isF  _               = False
        isO  (Operator _)    = True
        isO  _               = False
        isCl (Class _)       = True
        isCl _               = False
        isConst (Constant _) = True
        isConst _            = False
        collection = collect m



mapping :: [(String, SimpleInfo -> [(String,String)])]
mapping = [ 
            ("type",         mtypes) 
          , ("constructor",  mconstructors)
          , ("function",  mfunctions)
          , ("infixoperator", moperators)
          , ("class", mclasses)
          , ("constant", mconstants)
          ]
          
mtypes :: SimpleInfo -> [(String, String)]
mtypes SimpleInfo{types} = map dp types
moperators SimpleInfo{operators} = map (\ a -> (a, "\\ \\mathbin{"++ makeLatexSafe a++"}\\ ")) 
                                       operators
mconstructors SimpleInfo{constructors} = map (dp) constructors
mfunctions SimpleInfo{functions   } = map (dp) functions
mclasses SimpleInfo{classes}        = map (dp) classes
mconstants SimpleInfo{constants}    = map (dp) constants



fooz = [4, 13, 42]
douz = [4.0, 13.0, 42.0]



(<++>) :: a -> b -> a
(<++>) a b = a

tid :: Typeable a => a -> a
tid = id


fromParse (ParseOk m) = m



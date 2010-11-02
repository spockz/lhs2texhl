module Literate.SimpleInfo where

data SimpleInfo = SimpleInfo {
  fileName        :: String,
  moduleName      :: String,
  types           :: [String],
  constructors    :: [String],
  functions       :: [String],
  operators       :: [String],
  literalChars    :: [Char],
  literalStrings  :: [String]
} deriving Show

simpleinfo = SimpleInfo{ fileName = ""
                       , moduleName = ""
                       , types = []
                       , constructors = []
                       , functions = []
                       , operators = []
                       , literalChars   = []
                       , literalStrings = []
                       }
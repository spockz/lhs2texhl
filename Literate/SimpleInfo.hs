module Literate.SimpleInfo where

data SimpleInfo = SimpleInfo {
  fileName        :: String,
  moduleName      :: String,
  types           :: [String],
  constructors    :: [String],
  functions       :: [String],
  literalNumbers  :: [String],
  literalChars    :: [Char]
}

simpleinfo = SimpleInfo{ fileName = ""
                       , moduleName = ""
                       , types = []
                       , constructors = []
                       , functions = []
                       , literalNumbers = []
                       , literalChars = []
                       }
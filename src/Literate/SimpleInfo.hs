module Literate.SimpleInfo where

data SimpleInfo = SimpleInfo {
  fileName        :: String,
  moduleName      :: String,
  types           :: [String],
  constructors    :: [String],
  functions       :: [String],
  operators       :: [String],
  classes         :: [String]

  
} deriving Show

simpleinfo = SimpleInfo{ fileName = ""
                       , moduleName = ""
                       , types = []
                       , constructors = []
                       , functions = []
                       , operators = []
                       , classes   = []
                       }
                       
{-# LANGUAGE DeriveDataTypeable #-}
module Base.CLI (ProgramOptions(..), Action(..), usage, newCommands, standard, module System.Console.CmdArgs) where
  
import System.Console.CmdArgs

import Base.Common

data Action = Format | ListCommands
  deriving (Show, Typeable, Data)

instance Default Action where
  def = Format

data ProgramOptions = ProgramOptions
  { agda_mode :: Bool
  , action :: Action
  , input   :: [FilePath]
  , output  :: FilePath
  } deriving (Show, Data, Typeable)



usage :: String
usage = unlines 
          [ programName ++" "++ programVersion ++" - A lhs2TeX Syntax Colouring preprocessor"
          , "Consult the README file for extra information or visit:\n"
          , "    https://github.com/spockz/lhs2texhl"
          , "  and "
          , "    http://alessandrovermeulen.me/projects/lhs2texhl\n"
          , "Copyright 2010, Alessandro Vermeulen <me@alessandrovermeulen.me>" ]

newCommands :: String
newCommands = unlines [
    "\\newcommand{\\lhsCHfunction}[1]{\\color{infixoperator}{{#1}}}",
    "\\newcommand{\\lhsCHinfixoperator}[1]{\\color{infixoperator}{{#1}}}",
    "\\newcommand{\\lhsCHprelude}[1]{\\color{prelude}{{#1}}}",
    "\\newcommand{\\lhsCHkeyword}[1]{\\color{keyword}{{#1}}}",
    "\\newcommand{\\lhsCHconstructor}[1]{\\color{constructor}{{#1}}}",
    "\\newcommand{\\lhsCHtype}[1]{\\color{datatype}{{#1}}}",
    "\\newcommand{\\lhsCHsyntax}[1]{\\color{syntax}{{#1}}}",
    "\\newcommand{\\lhsCHclass}[1]{\\color{class}{{#1}}}",
    "\\newcommand{\\lhsCHconstant}[1]{\\color{constant}{{#1}}}"
  ]

-- | Standard command line options. 
--

standard = cmdArgsMode $ ProgramOptions 
            { 
              agda_mode     = def &= help "Run in agda-mode!"
            , action        = (def &= help "What should the program do? Format|ListCommands.") &= typ "Action"
            , output        = (def &= help "Output file") &= typFile
            , input         = (def &= args ) 
            } &= summary usage



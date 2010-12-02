{-# LANGUAGE DeriveDataTypeable #-}
module Base.CLI (ProgramOptions(..), usage, standard, module System.Console.CmdArgs) where
  
import System.Console.CmdArgs

import Base.Common

data ProgramOptions = ProgramOptions
  { agda_mode :: Bool
  , input   :: [FilePath]
  , output  :: FilePath
  } deriving (Show, Data, Typeable)



usage :: String
usage = unlines 
          [ programName ++" "++ programVersion ++" - A lhs2TeX Syntax Coloring pre-processor"
          , "Consult the README file for extra information or visit:\n"
          , "  https://github.com/spockz/lhs2texhl"
          , "Copyright 2010, Alessandro Vermeulen <me@alessandrovermeulen.me>" ]



-- | Standard command line options. 
--

standard = cmdArgsMode $ ProgramOptions 
            { 
              agda_mode  = def &= help "Run in agda-mode!"  
            , output     = (def &= help "Output file") &= typFile
            , input      = (def &= args ) 
            } &= summary usage



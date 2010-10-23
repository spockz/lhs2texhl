module Language.LaTeX where

import Data.String.Utils

makeLatexSafe :: String -> String
makeLatexSafe = replace ">" "\\textgreater" 
              . replace "<" "\\textless"
              . replace "_" "\\_"
              . replace "\"" "\\\""
              . replace "$" "\\$"
              . replace "&" "\\&"
              . replace "#" "\\#"
              
dp :: String -> (String, String)
dp a = (a, makeLatexSafe a)
module Language.LaTeX where


---------
import Data.String.Utils

import Language.Markup
---------


makeLatexSafe :: String -> String
makeLatexSafe = replace ">" "\\textgreater" 
              . replace "<" "\\textless"
              -- . replace "_" "\\_"
              . replace "\"" "\\\""
              
              . replace "&" "\\&"
              . replace "#" "\\#"

{- This part is to conserve some lhs2TeX behaviour. Really shouldn't be here... -}
              . replace "=" "\\ = \\ "
              -- . replace "_" "\\ \\anonymous\\ "
              . replace "$" "\\ \\mathbin{\\$}\\ "
              . replace "&&" "\\ \\mathrel{\\wedge}\\ "
              . replace "||" "\\ \\mathrel{\\vee}\\ "
              . replace "<-" "\\ \\leftarrow\\ "
              . replace "->" "\\ \\to\\  "
              . replace "=>" "\\ \\Rightarrow\\ "
              . replace "==" "\\ \\equiv\\ "
              . replace "++" "\\ \\plus\\ "
              . replace "/=" "\\ \\not\\equiv\\ "
              . replace "<=" "\\ \\leq\\ "
              . replace ">=" "\\ \\geq\\ "
              . replace ">>" "\\ \\sequ\\ "
              . replace ">>=" "\\ \\bind\\ "
              . replace "=<<" "\\ \\rbind\\ "
              . replace "undefined" "\\bot\\  "
              . replace "not" "\\ \\neg "
              
              
              -- %format _          = "\anonymous "
              -- %format ->         = "\to "
              -- %format <-         = "\leftarrow "
              -- %format =>         = "\Rightarrow "
              -- %format \          = "\lambda "
              -- %format |          = "\mid "
              
              . replace "{" "\\{"
              . replace "}" "\\}"
              
              . replace "\\" "\\lambda " -- argh

char :: Integer -> String
char n = "\\ \\char''" ++ show n ++"\\ "
              
dp :: String -> (String, String)
dp a = (a, (markup . makeLatexSafe) a)

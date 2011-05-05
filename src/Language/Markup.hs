module Language.Markup (markup) where

import Data.Char (isDigit)

import Data.String.Utils ( rtrim, ltrim )

markup :: String -> String
markup = m' . b . break (\ a -> a == '_' || isDigit a) . rtrim "_"
  where m' (str, [])   = str
        m' (str, sub)  = str ++ "_{" ++ sub ++ "}"
        b (a , b) = (a , ltrim "_" b)
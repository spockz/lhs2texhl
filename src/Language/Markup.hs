module Language.Markup (markup) where

import Data.Char (isDigit)

markup :: String -> String
markup = m' . b . break (\ a -> a == '_' || isDigit a) . rtrim "_"
  where m' (str, [])   = str
        m' (str, sub)  = str ++ "_{" ++ sub ++ "}"
        b (a , b) = (a , ltrim "_" b)
               


rtrim :: String -> String -> String
rtrim chars inp = rtrim' inp
  where
    rtrim' ""  = []
    rtrim' [x] = case elem x chars of
                   True  -> []
                   False -> [x]
    rtrim' (x:xs) = let tail = rtrim' xs
                    in
                      case tail of
                        []   -> case elem x chars of
                                  True  -> []
                                  False -> [x]
                        xs   -> x :  tail
                        
ltrim :: String -> String -> String
ltrim chars = ltrim'
  where ltrim' s = case s of
                     [] -> []
                     (x:xs) -> if elem x chars
                               then ltrim' xs
                               else s
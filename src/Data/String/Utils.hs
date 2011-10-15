module Data.String.Utils where

rtrim :: String -> String -> String
rtrim chars inp = rtrim' inp
  where
    rtrim' ""  = []
    rtrim' [x] = if x `elem` chars then [] else [x]
    rtrim' (x:xs) = let tail = rtrim' xs
                    in
                      case tail of
                        []   -> if x `elem` chars then [] else [x]
                        xs   -> x :  tail
                        
ltrim :: String -> String -> String
ltrim chars = ltrim'
  where ltrim' s = case s of
                     [] -> []
                     (x:xs) -> if x `elem` chars
                               then ltrim' xs
                               else s
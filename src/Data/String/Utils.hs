module Data.String.Utils where

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
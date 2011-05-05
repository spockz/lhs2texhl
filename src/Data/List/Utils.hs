{- |
   Module     : Data.List.Utils
   Copyright  : Copyright (C) 2004-2006 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with lists.

Written by John Goerzen, jgoerzen\@complete.org

----

Scavenged from the MissingH Data.List.Utils module, with only the things we need
here, in order to keep our dependencies to a minumum.
-}
module Data.List.Utils where
  
import Data.List ( intersperse, isPrefixOf )

{- | Returns true if the given list starts with the specified elements;
false otherwise.  (This is an alias for "Data.List.isPrefixOf".)

Example:

> startswith "He" "Hello" -> True

-}

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

{- | Similar to Data.List.span, but performs the test on the entire remaining
list instead of just one element. 

@spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@ 
-}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs
  
{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a delimiter and a list (or string), split into components.

Example:

> split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]

> split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
-}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)


{- | Given a list and a replacement list, replaces each occurance of the search
list with the replacement list in the operation list.

Example:

>replace "," "." "127,0,0,1" -> "127.0.0.1"

This could logically be thought of as:

>replace old new l = join new . split old $ l
-}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)


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
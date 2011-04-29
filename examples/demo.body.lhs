Data declarations:

\begin{code}
type Milk = [Char]  

newtype Silk = Silk Milk
  
data Foo a  =  Bar
            |  Cafe a
            |  Restaurant Int a
\end{code}

Classes and instances

\begin{code}
class Foo a where
  foo :: a -> Int -> a
\end{code}

\begin{code}
instance Foo Int where
  foo = flip const
\end{code}

Basic functions:

\begin{code}
sum :: Num a => [a] -> Integer
sum []      = 0
sum (x:xs)  = x + sum xs
\end{code}

Operators

\begin{code}
chained :: a b c -> a b d
chained = foo >>> bar >>> restaurant

(<+>) :: Num a => a -> a -> a
(<+>) = (+)
\end{code}
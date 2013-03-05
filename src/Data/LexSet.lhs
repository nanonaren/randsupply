\begin{code}

module Data.LexSet
{-# WARNING "Haven't considered perf fully." #-}
    (
      lexRank
    , lexUnrank
    , klexRank
    , klexUnrank
    , choose
    ) where

import Data.List (foldl')

-- | rank set
lexRank :: Integral a => a -> [a] -> a
lexRank n xs = foldl' (\acc x -> acc + 2^(x-1)) 0 xs

-- | unrank to set
--   elements listed in ascending order
lexUnrank :: Integral a => a -> a -> [a]
lexUnrank n r = lexUnrank' (n+1) 1 r
lexUnrank' m n r
    | n == m = []
    | otherwise =
        if r `mod` 2 == 1
        then n : lexUnrank' m (n+1) (r `div` 2)
        else lexUnrank' m (n+1) (r `div` 2)

klexUnrank :: Integral a => a -> a -> a -> [a]
klexUnrank k n r = help r 1 1
    where help r x i
            | i > k = []
            | otherwise = let f v y = let c = (n-y) `choose` (k-i)
                                      in if c <= v
                                         then f (v-c) (y+1)
                                         else (v,y)
                              (r',x') = f r x
                          in x' : help r' (x'+1) (i+1)

klexRank :: Integral a => a -> a -> [a] -> a
klexRank k n xs = help 1 (0:xs) 0
    where help i ys r
            | i > k = r
            | otherwise =
                let (y:z:ys') = ys
                    r' = if y + 1 <= z - 1
                         then foldl' (\acc j -> acc + ((n-j) `choose` (k-i)))
                                     r [y+1..z-1]
                         else r
            in r' `seq` help (i+1) (z:ys') r'

choose n k = mult [n,n-1..(n-k)+1] `div` mult [1..k]
    where mult xs = foldl' (*) 1 xs

\end{code}
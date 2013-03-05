\begin{code}

module Data.RandSupply.Set
    (
      randSubset
    , randkSubset
    ) where

import Data.RandSupply

import Data.LexSet

-- | get a random subset of a list (non-empty)
randSubset :: RandSupply m => Int -> [a] -> m [a]
randSubset n xs = do
  ls <- randLexset n
  return $ snd.unzip.filter (flip elem ls.fst) $ zip [1..] xs

-- | get a random subset of size k
randkSubset :: RandSupply m => Int -> Int -> [a] -> m [a]
randkSubset k n xs = do
  r <- randIntR (0,n `choose` k -1)
  let ls = klexUnrank k n r
  return $ snd.unzip.filter (flip elem ls.fst) $ zip [1..] xs

-- | get a subset of [1..n]
randLexset :: RandSupply m => Int -> m [Int]
randLexset n = randIntR (0,2^n-1) >>= return.lexUnrank n

\end{code}
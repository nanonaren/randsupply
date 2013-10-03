Random supply inferface. Fed up of so many random libraries
without a common interface.

** The random values returned must be distributed uniformly! **

Naren Sundar

\begin{code}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.RandSupply
    (
      RandSupply (..)
    , coinFlip
    , randIntR
    , sampleWR
    -- ** Re-imports
    , module Data.Prob
    ) where

import Data.Prob
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U

class (Functor m,Monad m) => RandSupply m where
    -- | Random in one of the ranges: [0,1], (0,1], [0,1) or (0,1)
    --   I may make the range requirement exact later
    randProb :: m Prob
    randInt :: m Int

\end{code}

Utilities
---------

\begin{code}

randIntR :: RandSupply m => (Int,Int) -> m Int
randIntR (l,r) = randInt >>= \v -> return $! (abs v `mod` (r-l+1)) + l

-- | binary decision
coinFlip :: RandSupply m => Prob -> m Bool
coinFlip p = randProb >>= \d -> return $! if d <= p then True else False

-- | sample k things without replacement from [0..n-1]
sampleWR :: RandSupply m => Int -> Int -> m (U.Vector Int)
sampleWR n k
    | n < k = error "sampleWR: n < k"
    | otherwise = do
        rs <- replicateM k $ randIntR (0,n-1)
        return $ U.unsafeTake k $
                 U.modify (\v -> loop v 0 n rs) (U.fromListN n [0..n-1]) 
    where loop _ _ _ [] = return ()
          loop v !off !sz (r:rs) = do
            let i = off + (r `mod` sz)
            MU.unsafeSwap v i off
            loop v (off+1) (sz-1) rs

\end{code}
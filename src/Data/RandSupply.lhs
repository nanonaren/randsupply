Random supply inferface. Fed up of so many random libraries
without a common interface.

** The random values returned must be distributed uniformly! **

Naren Sundar

\begin{code}

{-# LANGUAGE MagicHash #-}

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
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U

class Monad m => RandSupply m where
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
sampleWR :: RandSupply m => Int -> Int -> m [Int]
sampleWR n k = do
  rs <- sequence $ replicate k $ randIntR (0,n-1)
  return $ runST $ U.unsafeThaw (U.fromList [0..n-1]) >>= \v ->
                   loop v n rs []
    where loop _ _ [] acc = return acc
          loop v sz (r:rs) acc = do
            let i = r `mod` sz
            a <- MU.unsafeRead v i
            MU.unsafeSwap v i (sz-1)
            loop v (sz-1) rs (a:acc)

\end{code}
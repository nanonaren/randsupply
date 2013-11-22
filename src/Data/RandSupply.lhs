Random supply inferface. Fed up of so many random libraries
without a common interface.

** The random values returned must be distributed uniformly! **

Naren Sundar

\begin{code}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.RandSupply
    (
      RandSupply (..)
    , coinFlip
    , randIntR
    , sampleWR
    , fromDirichletU
    -- ** Re-imports
    , module Data.Prob
    ) where

import Data.Prob
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

class (Functor m,Monad m) => RandSupply m where
    -- | Random in one of the ranges: [0,1], (0,1], [0,1) or (0,1)
    --   I may make the range requirement exact later
    randProb :: m Prob
    randInt :: m Int
    randGamma :: Double -> Double -> m Double

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

Sample from a Dirichlet with a uniform parameter

\begin{code}

fromDirichletU :: RandSupply m => Int -> Double -> m (U.Vector Double)
fromDirichletU n alpha =
  normalize <$> U.replicateM n (randGamma alpha 1)

-- | use this from the VectorUtils package instead
normalize :: G.Vector v Double => v Double -> v Double
normalize v = let s = G.sum v
              in if s == 0
                 then error "normalize: total is zero"
                 else G.map (/s) v

\end{code}
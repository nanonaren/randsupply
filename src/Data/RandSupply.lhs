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
    -- ** Re-imports
    , module Data.Prob
    ) where

import Data.Prob

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

\end{code}
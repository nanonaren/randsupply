Random supply inferface. Fed up of so many random libraries
without a common interface.

** The random values returned must be distributed uniformly! **

Naren Sundar

\begin{code}

{-# LANGUAGE MagicHash #-}

module Data.RandSupply
    (
      RandSupply (..)
    , getBool
    ) where

import Data.Prob

class Monad m => RandSupply m where
    -- | Random in one of the ranges: [0,1], (0,1], [0,1) or (0,1)
    --   I may make the range requirement exact later
    randProb :: m Prob

\end{code}

Utilities
---------

\begin{code}

-- | binary decision
getBool :: RandSupply m => Prob -> m Bool
getBool p = randProb >>= \d -> return $! if d <= p then True else False

\end{code}
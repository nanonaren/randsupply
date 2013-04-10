Prob newtype
Naren Sundar

TODO: use MagicHash later to work with some unboxed stuff...
      look at vector package for reference.

\begin{code}

module Data.Prob
    (
      Prob
    , unProb
    , mkProb
    -- ** Unsafe functions
    , mkProbUnsafe
    ) where

newtype Prob = Prob {unProb :: Double}
    deriving (Show,Ord,Eq)

{-# INLINE mkProb #-}
mkProb :: Double -> Prob
mkProb p
    | 0 <= p && p <= 1 = Prob p
    | otherwise = error "mkProb: out of bounds"

{-# INLINE mkProbUnsafe #-}
mkProbUnsafe :: Double -> Prob
mkProbUnsafe = Prob

\end{code}
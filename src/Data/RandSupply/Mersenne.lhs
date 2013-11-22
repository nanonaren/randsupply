RandSupply for Mersenne
Naren Sundar

\begin{code}

module Data.RandSupply.Mersenne
    (
      module Control.Monad.Mersenne.Random
    , module System.Random.Mersenne.Pure64
    ) where

import Data.RandSupply
import Control.Monad (liftM)
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64

import Data.Prob

instance RandSupply Rand where
    -- | Not sure range requirement is satisfied
    randProb = getDouble >>= return.mkProbUnsafe
    randInt = getInt
    randGamma = error "Rand: randGamma not implemented"

instance Functor Rand where
    fmap f m = liftM f m

\end{code}
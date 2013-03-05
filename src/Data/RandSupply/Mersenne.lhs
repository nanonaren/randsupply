RandSupply for Mersenne
Naren Sundar

\begin{code}

module Data.RandSupply.Mersenne
    (
    ) where

import Data.RandSupply
import Control.Monad.Mersenne.Random

import Data.Prob

instance RandSupply Rand where
    -- | Not sure range requirement is satisfied
    randProb = getDouble >>= return.mkProbUnsafe

\end{code}
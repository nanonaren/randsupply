Naren Sundar

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Data.RandSupply.Randable
>     (
>       Randable (..)
>     , module Data.RandSupply
>     ) where
>
> import Control.Monad
> import Control.Monad.IO.Class
> import Data.RandSupply
> import System.Random.MWC

A class instance for a monad that can provide access to GenIO.

> class (Functor m,MonadIO m) => Randable m where
>     getGen :: m GenIO

A monad that satisfies Randable can be made into a RandSupply

> instance Randable m => RandSupply m where
>     {-# INLINE randProb #-}
>     randProb = getGen >>= fmap mkProb.liftIO.uniform
>     {-# INLINE randInt #-}
>     randInt = getGen >>= liftIO.uniform
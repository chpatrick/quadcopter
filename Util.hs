module Util where

import Control.Applicative
import Data.Word
import Control.Monad
import System.Clock

(.>) :: a -> (a -> b) -> b
x .> f = f x

infixr 0 .> 

getMillis :: IO Word64
getMillis = (\(TimeSpec s n) -> fromIntegral s * 1000 + fromIntegral n `div` 1000000) <$> getTime Monotonic

(>.) :: Monad m => (a -> m b) -> (a -> m c) -> (a -> m c)
(>.) m m' x = m x >> m' x

infixl 1 >.

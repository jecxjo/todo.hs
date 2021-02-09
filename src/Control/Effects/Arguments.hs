{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Effects.Arguments (
  getArgs
  ) where

import           Control.Algebra (Algebra(..), Has, (:+:)(..), send)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified System.Environment as IO

data Arguments (m :: Type -> Type) k where
  GetArgs :: Arguments m [String]

getArgs :: Has Arguments sig m => m [String]
getArgs = send GetArgs

newtype ArgumentsIOC m a = ArgumentsIOC { runArgumentsIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Arguments :+: sig) (ArgumentsIOC m) where
  alg hdl sig ctx = case sig of
    L GetArgs -> (<$ ctx) <$> liftIO IO.getArgs
    R other -> ArgumentsIOC (alg (runArgumentsIO . hdl) other ctx)

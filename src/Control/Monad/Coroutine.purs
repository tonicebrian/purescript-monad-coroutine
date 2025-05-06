module Control.Monad.Coroutine where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

newtype Coroutine s m r = Coroutine (m (Either (s (Coroutine s m r)) r))

resume :: forall s m r. Coroutine s m r -> m (Either (s (Coroutine s m r)) r)
resume (Coroutine m) = m

type CoroutineStepResult s m r = Either (s (Coroutine s m r)) r

instance functorCoroutine :: (Functor s, Functor m) => Functor (Coroutine s m) where
  map f co = Coroutine (map (apply f) (resume co))
    where
      apply fc (Right x) = Right (fc x)
      apply fc (Left s) = Left (map (map fc) s)

instance applyCoroutine :: (Monad m, Functor m, Functor s) => Apply (Coroutine s m) where
  apply (Coroutine mf) (Coroutine mx) = Coroutine do
    fStep <- mf
    xStep <- mx
    case fStep of
      Right f -> do
        xStep <- mx
        case xStep of
          Right x -> pure (Right (f x))
          Left s  -> pure (Left (map (\next -> Coroutine (pure (Right f)) <*> next) s))
      Left s -> pure (Left (map (\next -> next <*> Coroutine mx) s))

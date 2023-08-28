{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | 'HoistFail extends 'MonadFail with 'hoistFail, which enables lifting
-- of partiality types such as 'Maybe' and @'Either' e@ into the monad.
module Control.Monad.Fail.Hoist
  ( HoistFail(..)
  , hoistFail'
  , hoistFailE
  , hoistFailE'
  , hoistFailM
  , hoistFailM'
  , hoistFailEM
  , hoistFailEM'
  , (<%#>)
  , (<%!#>)
  , (<#>)
  , (<!#>)
  ) where

import           Prelude hiding (fail)

import           Control.Monad              ((<=<))
import           Control.Monad.Fail         (MonadFail (..))

import           Data.Either                (Either, either)

#if MIN_VERSION_mtl(2,2,2)
import           Control.Monad.Except       (Except, ExceptT, runExcept,
                                             runExceptT)
#else
import           Control.Monad.Error        (Error, ErrorT, runErrorT)
#endif

#if MIN_VERSION_either(5,0,0)
-- Control.Monad.Trans.Either was removed from @either@ in version 5.
#else
import           Control.Monad.Trans.Either (EitherT, eitherT, runEitherT)
#endif

class Monad m => HoistFail t m e | t -> e where

  -- | Given a conversion from the error in @t a@ to @String@, we can hoist the
  -- computation into @m@.
  --
  -- @
  -- 'hoistFail' :: 'MonadFail' m => (() -> e) -> 'Maybe'       a -> m a
  -- 'hoistFail' :: 'MonadFail' m => (a  -> e) -> 'Either'  a   b -> m b
  -- @
  hoistFail
    :: (e -> String)
    -> t a
    -> m a

instance MonadFail m => HoistFail Maybe m () where
  hoistFail f = maybe (fail $ f ()) return

instance MonadFail m => HoistFail (Either e) m e where
  hoistFail f = either (fail . f) return

#if MIN_VERSION_either(5,0,0)
-- Control.Monad.Trans.Either was removed from @either@ in version 5.
#else
instance (m ~ n, MonadFail m) => HoistFail (EitherT e n) m e where
  hoistFail f = eitherT (fail . f) return
#endif

#if MIN_VERSION_mtl(2,2,2)
instance MonadFail m => HoistFail (Except e) m e where
  hoistFail f = either (fail . f) return . runExcept

instance MonadFail m => HoistFail (ExceptT e m) m e where
  hoistFail f = either (fail . f) return <=< runExceptT
#else
-- 'ErrorT' was renamed to 'ExceptT' in mtl 2.2.2
instance MonadError m => HoistFail (ErrorT e m) m e where
  hoistFail f = either (fail . f) return <=< runErrorT
#endif

hoistFail' :: HoistFail t m String => t a -> m a
hoistFail' = hoistFail id

hoistFailE :: MonadFail m => (e -> String) -> Either e a -> m a
hoistFailE = hoistFail

hoistFailE' :: MonadFail m => Either String a -> m a
hoistFailE' = hoistFail'

hoistFailM
  :: HoistFail t m e
  => (e -> String)
  -> m (t a)
  -> m a
hoistFailM e m = do
  x <- m
  hoistFail e x

hoistFailM'
  :: HoistFail t m String
  => m (t a)
  -> m a
hoistFailM' m = do
  x <- m
  hoistFail' x

hoistFailEM
  :: MonadFail m
  => (e -> String)
  -> m (Either e a)
  -> m a
hoistFailEM = hoistFailM

hoistFailEM'
  :: MonadFail m
  => m (Either String a)
  -> m a
hoistFailEM' = hoistFailM'

-- | A flipped synonym for 'hoistFail'.
--
-- @
-- ('<%#>') :: 'MonadFail' m => 'Maybe'       a -> (() -> e) ->           m a
-- ('<%#>') :: 'MonadFail' m => 'Either'  a   b -> (a  -> e) ->           m b
-- @
(<%#>)
  :: HoistFail t m e
  => t a
  -> (e -> String)
  -> m a
(<%#>) = flip hoistFail

infixl 8 <%#>
{-# INLINE (<%#>) #-}

-- | A flipped synonym for 'hoistFailM'.
--
-- @
-- ('<%!#>') :: 'MonadError' e m => m ('Maybe'       a) -> (() -> e) ->           m a
-- ('<%!#>') :: 'MonadError' e m => m ('Either'  a   b) -> (a  -> e) ->           m b
-- ('<%!#>') :: 'MonadError' e m =>    'ExceptT' a m b  -> (a  -> e) -> 'ExceptT' a m b
-- @
(<%!#>)
  :: HoistFail t m e
  => m (t a)
  -> (e -> String)
  -> m a
(<%!#>) = flip hoistFailM

infixl 8 <%!#>
{-# INLINE (<%!#>) #-}

-- | A version of '<%#>' that ignores the error in @t a@ and replaces it
-- with a new one.
--
-- @
-- ('<#>') :: 'MonadFail' m => 'Maybe'       a -> String ->           m a
-- ('<#>') :: 'MonadFail' m => 'Either'  a   b -> String ->           m b
-- @
(<#>)
  :: HoistFail t m String
  => t a
  -> String
  -> m a
m <#> e = m <%#> const e

infixl 8 <#>
{-# INLINE (<#>) #-}

-- | A version of '<#>' that operates on values already in the monad.
--
-- @
-- ('<!#>') :: 'MonadFail m => m ('Maybe'       a) -> String ->           m a
-- ('<!#>') :: 'MonadFail m => m ('Either'  a   b) -> String ->           m b
-- ('<!#>') :: 'MonadFail m =>    'ExceptT' a m b  -> String -> 'ExceptT' a m b
-- @
(<!#>)
  :: HoistFail t m String
  => m (t a)
  -> String
  -> m a
m <!#> e = do
  x <- m
  x <#> e

infixl 8 <!#>
{-# INLINE (<!#>) #-}

{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | 'HoistFail' extends 'MonadFail' with 'hoistFail', which enables lifting
-- of partiality types such as 'Maybe' and 'Either' e@ into the monad.
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

class Monad m => HoistFail m t e | t -> e where

  -- | Given a conversion from the error in @t a@ to @String@, we can hoist the
  -- computation into @m@.
  --
  -- @
  -- 'hoistFail' :: 'MonadFail' m => (() -> String) -> 'Maybe'       a -> m a
  -- 'hoistFail' :: 'MonadFail' m => (a  -> String) -> 'Either'  a   b -> m b
  -- @
  hoistFail
    :: (e -> String)
    -> t a
    -> m a

instance MonadFail m => HoistFail m Maybe () where
  hoistFail f = maybe (fail $ f ()) return

instance MonadFail m => HoistFail m (Either e) e where
  hoistFail f = either (fail . f) return

#if MIN_VERSION_either(5,0,0)
-- Control.Monad.Trans.Either was removed from @either@ in version 5.
#else
instance (m ~ n, MonadFail m) => HoistFail m (EitherT e n) e where
  hoistFail f = eitherT (fail . f) return
#endif

#if MIN_VERSION_mtl(2,2,2)
instance MonadFail m => HoistFail m (Except e) e where
  hoistFail f = either (fail . f) return . runExcept

instance MonadFail m => HoistFail m (ExceptT e m) e where
  hoistFail f = either (fail . f) return <=< runExceptT
#else
-- 'ErrorT' was renamed to 'ExceptT' in mtl 2.2.2
instance MonadError m => HoistFail m (ErrorT e m) e where
  hoistFail f = either (fail . f) return <=< runErrorT
#endif

-- | Hoist computations whose error type is already 'String'.
hoistFail' :: HoistFail m t String => t a -> m a
hoistFail' = hoistFail id

-- | 'hoistFail' specialised to @Either@. Helpful for using functions that are
-- polymorphic in their monad.
--
-- You could consider this as having the following type:
-- @
-- hoistFailE :: (MonadError e m', MonadFail m) => (e -> String) -> m' a -> m a
-- @
hoistFailE :: MonadFail m => (e -> String) -> Either e a -> m a
hoistFailE = hoistFail

-- | 'hoistFail'' specialised to @Either@. Helpful for using functions that are
-- polymorphic in their monad.
--
-- You could consider this as having the following type:
-- @
-- hoistFailE' :: (MonadError String m', MonadFail m) => m' a -> m a
-- @
hoistFailE' :: MonadFail m => Either String a -> m a
hoistFailE' = hoistFail'

-- | A version of 'hoistFail' that operates on values already in the monad.
--
-- @
-- 'hoistFailM' :: 'MonadFail' m => (() -> String) -> m ('Maybe'       a) ->           m a
-- 'hoistFailM' :: 'MonadFail' m => (a  -> String) -> m ('Either'  a   b) ->           m b
-- 'hoistFailM' :: 'MonadFail' m => (a  -> String) ->    'ExceptT' a m b  -> 'ExceptT' a m b
-- @
hoistFailM
  :: HoistFail m t e
  => (e -> String)
  -> m (t a)
  -> m a
hoistFailM e m = hoistFail e =<< m

-- | A version of 'hoistFail'' that operates on values already in the monad.
--
-- @
-- 'hoistFailM'' :: 'MonadFail' m => m ('Maybe'       a) ->           m a
-- 'hoistFailM'' :: 'MonadFail' m => m ('Either'  a   b) ->           m b
-- 'hoistFailM'' :: 'MonadFail' m =>    'ExceptT' a m b  -> 'ExceptT' a m b
-- @
hoistFailM'
  :: HoistFail m t String
  => m (t a)
  -> m a
hoistFailM' m = hoistFail' =<< m

-- | A version of 'hoistFailE' that operates on values already in the monad.
hoistFailEM
  :: MonadFail m
  => (e -> String)
  -> m (Either e a)
  -> m a
hoistFailEM = hoistFailM

-- | A version of 'hoistFailE'' that operates on values already in the monad.
hoistFailEM'
  :: MonadFail m
  => m (Either String a)
  -> m a
hoistFailEM' = hoistFailM'

-- | A flipped synonym for 'hoistFail'. Mnemonic: @#@ looks a bit like @F@
--
-- @
-- ('<%#>') :: 'MonadFail' m => 'Maybe'       a -> (() -> e) ->           m a
-- ('<%#>') :: 'MonadFail' m => 'Either'  a   b -> (a  -> e) ->           m b
-- @
(<%#>)
  :: HoistFail m t e
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
  :: HoistFail m t e
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
  :: HoistFail m t e
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
  :: HoistFail m t e
  => m (t a)
  -> String
  -> m a
m <!#> e = hoistFail (const e) =<< m

infixl 8 <!#>
{-# INLINE (<!#>) #-}

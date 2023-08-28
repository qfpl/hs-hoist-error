{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | 'HoistError' extends 'MonadError' with 'hoistError', which enables lifting
-- of partiality types such as 'Maybe' and @'Either' e@ into the monad.
--
-- For example, consider the following @App@ monad that may throw @BadPacket@
-- errors:
--
-- @
-- data AppError = BadPacket 'String'
--
-- newtype App a = App ('EitherT' AppError 'IO') a
--  deriving ('Functor', 'Applicative', 'Monad', 'MonadError' AppError, 'MonadIO')
-- @
--
-- We may have an existing function that parses a 'String' into a @'Maybe' Packet@
--
-- @
-- parsePacket :: 'String' -> 'Maybe' Packet
-- @
--
-- which can be lifted into the @App@ monad with 'hoistError'
--
-- @
-- appParsePacket :: 'String' -> 'App' Packet
-- appParsePacket s = 'hoistError' (\\() -> BadPacket "no parse") (parsePacket s)
-- @
--
-- Similar instances exist for @'Either' e@ and @'EitherT' e m@.

module Control.Monad.Error.Hoist
  ( HoistError(..)
  , hoistError'
  , hoistErrorE
  , hoistErrorE'
  , hoistErrorM
  , hoistErrorM'
  , hoistErrorEM
  , hoistErrorEM'
  , (<%?>)
  , (<%!?>)
  , (<?>)
  , (<!?>)
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Error.Class  (MonadError (..))

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

-- | A tricky class for easily hoisting errors out of partiality types (e.g.
-- 'Maybe', @'Either' e@) into a monad. The parameter @e@ represents the error
-- information carried by the partiality type @t@, and @e'@ represents the type
-- of error expected in the monad @m@.
--
class Monad m => HoistError m t e e' | t -> e where

  -- | Given a conversion from the error in @t a@ to @e'@, we can hoist the
  -- computation into @m@.
  --
  -- @
  -- 'hoistError' :: 'MonadError' e m => (() -> e) -> 'Maybe'       a -> m a
  -- 'hoistError' :: 'MonadError' e m => (a  -> e) -> 'Either'  a   b -> m b
  -- 'hoistError' :: 'MonadError' e m => (a  -> e) -> 'ExceptT' a m b -> m b
  -- @
  hoistError
    :: (e -> e')
    -> t a
    -> m a

instance MonadError e m => HoistError m Maybe () e where
  hoistError f = maybe (throwError $ f ()) return

instance MonadError e' m => HoistError m (Either e) e e' where
  hoistError f = either (throwError . f) return

#if MIN_VERSION_either(5,0,0)
-- Control.Monad.Trans.Either was removed from @either@ in version 5.
#else
instance (m ~ n, MonadError e' m) => HoistError m (EitherT e n) e e' where
  hoistError f = eitherT (throwError . f) return
#endif

#if MIN_VERSION_mtl(2,2,2)
instance MonadError e' m => HoistError m (Except e) e e' where
  hoistError f = either (throwError . f) return . runExcept

instance MonadError e' m => HoistError m (ExceptT e m) e e' where
  hoistError f = either (throwError . f) return <=< runExceptT
#else
-- 'ErrorT' was renamed to 'ExceptT' in mtl 2.2.2
instance MonadError e' m => HoistError m (ErrorT e m) e e' where
  hoistError f = either (throwError . f) return <=< runErrorT
#endif

-- | Hoist computations whose error type is already @e@.
hoistError' :: HoistError m t e e => t a -> m a
hoistError' = hoistError id

-- | 'hoistError' specialised to @Either@. Helpful for using functions that are
-- polymorphic in their monad.
--
-- You could consider this as having the following type:
-- @
-- hoistErrorE :: (MonadError e m', MonadError e' m) => (e -> e') -> m' a -> m a
-- @
hoistErrorE
  :: MonadError e' m
  => (e -> e')
  -> Either e a
  -> m a
hoistErrorE = hoistError

-- | 'hoistError'' specialised to @Either@. Helpful for using functions that are
-- polymorphic in their monad.
--
-- You could consider this as having the following type:
-- @
-- hoistErrorE' :: (MonadError e m', MonadError e m) => m' a -> m a
-- @
hoistErrorE'
  :: MonadError e m
  => Either e a
  -> m a
hoistErrorE' = hoistErrorE id

-- | A version of 'hoistError' that operates on values already in the monad.
--
-- @
-- 'hoistErrorM' :: 'MonadError' e m => (() -> e) -> m ('Maybe'       a) ->           m a
-- 'hoistErrorM' :: 'MonadError' e m => (a  -> e) -> m ('Either'  a   b) ->           m b
-- 'hoistErrorM' :: 'MonadError' e m => (a  -> e) ->    'ExceptT' a m b  -> 'ExceptT' a m b
-- @
hoistErrorM
  :: HoistError m t e e'
  => (e -> e')
  -> m (t a)
  -> m a
hoistErrorM e m = do
  x <- m
  hoistError e x

-- | A version of 'hoistError'' that operates on values already in the monad.
--
-- @
-- 'hoistErrorM'' :: 'MonadError' () m => m ('Maybe'       a) ->           m a
-- 'hoistErrorM'' :: 'MonadError' e m => m ('Either'  e   b) ->           m b
-- 'hoistErrorM'' :: 'MonadError' e m => 'ExceptT' e m b  -> 'ExceptT' e m b
-- @
hoistErrorM'
  :: HoistError m t e e
  => m (t a)
  -> m a
hoistErrorM' m = do
  x <- m
  hoistError' x

-- | A version of 'hoistErrorE' that operates on values already in the monad.
hoistErrorEM
  :: MonadError e' m
  => (e -> e')
  -> m (Either e a)
  -> m a
hoistErrorEM = hoistErrorM

-- | A version of 'hoistErrorE'' that operates on values already in the monad.
hoistErrorEM'
  :: MonadError e m
  => m (Either e a)
  -> m a
hoistErrorEM' = hoistErrorM'

-- | A flipped synonym for 'hoistError'.
--
-- @
-- ('<%?>') :: 'MonadError' e m => 'Maybe'       a -> (() -> e) ->           m a
-- ('<%?>') :: 'MonadError' e m => 'Either'  a   b -> (a  -> e) ->           m b
-- ('<%?>') :: 'MonadError' e m => 'ExceptT' a m b -> (a  -> e) -> 'ExceptT' a m b
-- @
(<%?>)
  :: HoistError m t e e'
  => t a
  -> (e -> e')
  -> m a
(<%?>) = flip hoistError

infixl 8 <%?>
{-# INLINE (<%?>) #-}

-- | A flipped synonym for 'hoistErrorM'.
--
-- @
-- ('<%!?>') :: 'MonadError' e m => m ('Maybe'       a) -> (() -> e) ->           m a
-- ('<%!?>') :: 'MonadError' e m => m ('Either'  a   b) -> (a  -> e) ->           m b
-- ('<%!?>') :: 'MonadError' e m =>    'ExceptT' a m b  -> (a  -> e) -> 'ExceptT' a m b
-- @
(<%!?>)
  :: HoistError m t e e'
  => m (t a)
  -> (e -> e')
  -> m a
(<%!?>) = flip hoistErrorM

infixl 8 <%!?>
{-# INLINE (<%!?>) #-}

-- | A version of '<%?>' that ignores the error in @t a@ and replaces it
-- with a new one.
--
-- @
-- ('<?>') :: 'MonadError' e m => 'Maybe'       a -> e ->           m a
-- ('<?>') :: 'MonadError' e m => 'Either'  a   b -> e ->           m b
-- ('<?>') :: 'MonadError' e m => 'ExceptT' a m b -> e -> 'ExceptT' a m b
-- @
(<?>)
  :: HoistError m t e e'
  => t a
  -> e'
  -> m a
m <?> e = m <%?> const e

infixl 8 <?>
{-# INLINE (<?>) #-}

-- | A version of '<?>' that operates on values already in the monad.
--
-- @
-- ('<!?>') :: 'MonadError' e m => m ('Maybe'       a) -> e ->           m a
-- ('<!?>') :: 'MonadError' e m => m ('Either'  a   b) -> e ->           m b
-- ('<!?>') :: 'MonadError' e m =>    'ExceptT' a m b  -> e -> 'ExceptT' a m b
-- @
(<!?>)
  :: HoistError m t e e'
  => m (t a)
  -> e'
  -> m a
m <!?> e = do
  x <- m
  x <?> e

infixl 8 <!?>
{-# INLINE (<!?>) #-}

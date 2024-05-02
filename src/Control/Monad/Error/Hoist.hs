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
  , hoistErrorM
  , (<%?>)
  , (<%!?>)
  , (<?>)
  , (<!?>)
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Error.Class  (MonadError (..))

import           Data.Either                (Either, either)

import           Control.Monad.Except       (Except, ExceptT, runExcept,
                                             runExceptT)

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
  -- 'hoistError' :: 'MonadError' e m -> (() -> e) -> 'Maybe'       a -> m a
  -- 'hoistError' :: 'MonadError' e m -> (a  -> e) -> 'Either'  a   b -> m b
  -- 'hoistError' :: 'MonadError' e m -> (a  -> e) -> 'ExceptT' a m b -> m b
  -- @
  hoistError
    :: (e -> e')
    -> t a
    -> m a

instance MonadError e m => HoistError m Maybe () e where
  hoistError f = maybe (throwError $ f ()) return

instance MonadError e' m => HoistError m (Either e) e e' where
  hoistError f = either (throwError . f) return

instance MonadError e' m => HoistError m (Except e) e e' where
  hoistError f = either (throwError . f) return . runExcept

instance MonadError e' m => HoistError m (ExceptT e m) e e' where
  hoistError f = either (throwError . f) return <=< runExceptT

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

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | This module provides helper functions for lifting partiality types into
-- error-carrying monads like 'ExceptT'.
--
-- For example, consider the following @App@ monad that may throw @BadPacket@
-- errors:
--
-- @
-- data AppError = BadPacket 'Text'
--
-- newtype App a = App ('EitherT' AppError 'IO') a
--  deriving ('Functor', 'Applicative', 'Monad', 'MonadError' AppError, 'MonadIO')
-- @
--
-- We may have an existing function that attempts to parse a 'ByteString':
--
-- @
-- parsePacket :: 'ByteString' -> 'Either' 'Text' Packet
-- @
--
-- We can be lift this error into the @App@ monad using @('<%?>')@:
--
-- @
-- appParsePacket :: 'ByteString' -> 'App' Packet
-- appParsePacket s = parsePacket s \<%?\> BadPacket
-- @
--
-- Instances also exist for extracting errors from other partiality types
-- like @'Either' e@ and @'ExceptT' e m@.

module Control.Monad.Error.Hoist
  ( hoistError
  , hoistErrorM
  -- ** Operators
  -- $mnemonics
  , (<%?>)
  , (<%!?>)
  , (<?>)
  , (<!?>)
  -- * Helper class
  , PluckError(..)
  ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Error.Class  (MonadError (..))

import           Data.Either                (Either, either)

import           Control.Monad.Except       (Except, ExceptT, runExcept,
                                             runExceptT)

-- | Given a conversion from the error in @t a@ to @e'@, we can hoist the
-- computation into @m@.
--
-- @
-- 'hoistError' :: 'MonadError' e m -> (() -> e) -> 'Maybe'       a -> m a
-- 'hoistError' :: 'MonadError' e m -> (a  -> e) -> 'Either'  a   b -> m b
-- 'hoistError' :: 'MonadError' e m -> (a  -> e) -> 'ExceptT' a m b -> m b
-- @
hoistError
  :: (PluckError e t m, MonadError e' m)
  => (e -> e')
  -> t a
  -> m a
hoistError f = foldError (throwError . f) pure

-- | A version of 'hoistError' that operates on values already in the monad.
--
-- @
-- 'hoistErrorM' :: 'MonadError' e m => (() -> e) -> m ('Maybe'       a) ->           m a
-- 'hoistErrorM' :: 'MonadError' e m => (a  -> e) -> m ('Either'  a   b) ->           m b
-- 'hoistErrorM' :: 'MonadError' e m => (a  -> e) ->    'ExceptT' a m b  -> 'ExceptT' a m b
-- @
hoistErrorM
  :: (PluckError e t m, MonadError e' m)
  => (e -> e')
  -> m (t a)
  -> m a
hoistErrorM e m = m >>= hoistError e

-- $mnemonics
--
-- The operators in this package are named according to a scheme:
--
-- * @('<?>')@ is the simplest error-handling function: it replaces
--   any error with its second argument.
--
-- * The additional @!@ in @('<!?>')@ and @('<%!?>')@ means the
--   operator handles values that are already "in a monad".
--
-- * The additional @%@ in @('<%?>')@ and @('<%!?>')@ means the
--   operator takes a function argument, which is applies to the error
--   from the partiality type. (The mnemonic is that @%@ sometimes
--   means "mod", and we use "mod" as a shorthand for "modify". It's a
--   long bow, but @lens@ uses the same mnemonic.)

-- | A flipped synonym for 'hoistError'.
--
-- @
-- ('<%?>') :: 'MonadError' e m => 'Maybe'       a -> (() -> e) ->           m a
-- ('<%?>') :: 'MonadError' e m => 'Either'  a   b -> (a  -> e) ->           m b
-- ('<%?>') :: 'MonadError' e m => 'ExceptT' a m b -> (a  -> e) -> 'ExceptT' a m b
-- @
(<%?>)
  :: (PluckError e t m, MonadError e' m)
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
  :: (PluckError e t m, MonadError e' m)
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
  :: (PluckError e t m, MonadError e' m)
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
  :: (PluckError e t m, MonadError e' m)
  => m (t a)
  -> e'
  -> m a
m <!?> e = do
  x <- m
  x <?> e

infixl 8 <!?>
{-# INLINE (<!?>) #-}

-- | A class for plucking an error @e@ out of a partiality type @t@.
class PluckError e t m | t -> e where
  pluckError :: t a -> m (Either e a)
  default pluckError :: Applicative m => t a -> m (Either e a)
  pluckError = foldError (pure . Left) (pure . Right)

  foldError :: (e -> m r) -> (a -> m r) -> t a -> m r
  default foldError :: Monad m => (e -> m r) -> (a -> m r) -> t a -> m r
  foldError f g = either f g <=< pluckError

  {-# MINIMAL pluckError | foldError #-}

instance (Applicative m, e ~ ()) => PluckError e Maybe m where
  pluckError = pure . maybe (Left ()) Right
  foldError f = maybe (f ())

instance Applicative m => PluckError e (Either e) m where
  pluckError = pure
  foldError = either

instance Monad m => PluckError e (ExceptT e m) m where
  pluckError = runExceptT
  foldError f g = either f g <=< runExceptT

instance Applicative m => PluckError e (Except e) m where
  pluckError = pure . runExcept
  foldError f g = either f g . runExcept

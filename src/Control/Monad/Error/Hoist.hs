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
-- We can lift this error into the @App@ monad using @('<%?>')@:
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
  , hoistError'
  , hoistErrorM
  , hoistErrorM'
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
-- 'hoistError' :: 'MonadError' err m -> (() -> err) -> 'Maybe'       a -> m a
-- 'hoistError' :: 'MonadError' err m -> (e  -> err) -> 'Either'  e   a -> m a
-- 'hoistError' :: 'MonadError' err m -> (e  -> err) -> 'ExceptT' e m a -> m a
-- @
hoistError
  :: (PluckError e t m, MonadError e' m)
  => (e -> e')
  -> t a
  -> m a
hoistError f = foldError (throwError . f) pure

-- | @hoistError' = hoistError id@
--
-- @since 0.3.1.0
hoistError'
  :: (PluckError e t m, MonadError e m)
  => t a
  -> m a
hoistError' = hoistError id

-- | A version of 'hoistError' that operates on values already in the monad.
--
-- @
-- 'hoistErrorM' :: 'MonadError' err m => (() -> err) -> m ('Maybe'       a) -> m a
-- 'hoistErrorM' :: 'MonadError' err m => (e  -> err) -> m ('Either'  e   b) -> m a
-- 'hoistErrorM' :: 'MonadError' err m => (e  -> err) -> m  'ExceptT' e m b  -> m a
-- @
hoistErrorM
  :: (PluckError e t m, MonadError e' m)
  => (e -> e')
  -> m (t a)
  -> m a
hoistErrorM e m = m >>= hoistError e

-- | @hoistErrorM' = hoistErrorM id@
--
-- @since 0.3.1.0
hoistErrorM'
  :: (PluckError e t m, MonadError e m)
  => m (t a)
  -> m a
hoistErrorM' = hoistErrorM id

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
--   operator takes a function argument, which it applies to the error
--   from the partiality type. (The mnemonic is that @%@ sometimes
--   means "mod", and we abuse "mod" as a shorthand for "modify". It's a
--   long bow, but @lens@ uses the same mnemonic.)

-- | A flipped synonym for 'hoistError'.
--
-- @
-- ('<%?>') :: 'MonadError' err m => 'Maybe'       a -> (() -> err) -> m a
-- ('<%?>') :: 'MonadError' err m => 'Either'  e   a -> (e  -> err) -> m a
-- ('<%?>') :: 'MonadError' err m => 'ExceptT' e m a -> (e  -> err) -> m a
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
-- ('<%!?>') :: 'MonadError' err m => m ('Maybe'       a) -> (() -> err) ->           m a
-- ('<%!?>') :: 'MonadError' err m => m ('Either'  e   a) -> (e  -> err) ->           m a
-- ('<%!?>') :: 'MonadError' err m =>    'ExceptT' e m a  -> (e  -> err) -> 'ExceptT' e m a
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
-- ('<?>') :: 'MonadError' err m => 'Maybe'       a -> err -> m a
-- ('<?>') :: 'MonadError' err m => 'Either'  e   a -> err -> m a
-- ('<?>') :: 'MonadError' err m => 'ExceptT' e m a -> err -> m a
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
-- ('<!?>') :: 'MonadError' err m => m ('Maybe'       a) -> e -> m a
-- ('<!?>') :: 'MonadError' err m => m ('Either'  e   a) -> e -> m a
-- ('<!?>') :: 'MonadError' err m => m  'ExceptT' e m a  -> e -> m a
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
--
-- @since 0.3.0.0
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

instance (Applicative m, e ~ e') => PluckError e (Either e') m where
  pluckError = pure
  foldError = either

instance (Monad m, e ~ e') => PluckError e (ExceptT e' m) m where
  pluckError = runExceptT
  foldError f g = either f g <=< runExceptT

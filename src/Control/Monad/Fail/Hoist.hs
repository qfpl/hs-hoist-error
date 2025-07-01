{-# LANGUAGE FlexibleContexts #-}

-- | This module provides helpers for converting partiality types into
-- 'MonadFail' computations.
--
-- 'MonadFail''s purpose is to handle pattern-match failures in
-- @do@-expressions, and not to be a general-purpose error-handling
-- mechanism. Despite this, some libraries use it as one, and this
-- module can help you report errors via 'MonadFail'.
--
-- The operator mnemonics are the same as in
-- "Control.Monad.Error.Hoist", but with @#@ in place of @?@. You can
-- imagine a hastily-written @F@ looking kinda-sorta like a @#@, if it
-- helps.

module Control.Monad.Fail.Hoist
  ( hoistFail
  , hoistFail'
  , hoistFailM
  , hoistFailM'
  -- ** Operators
  , (<%#>)
  , (<%!#>)
  , (<#>)
  , (<!#>)
  ) where

import           Control.Monad.Error.Hoist  (PluckError(..))

-- | Given a conversion from the error in @t a@ to @String@, we can hoist the
-- computation into @m@.
--
-- @
-- 'hoistFail' :: 'MonadFail' m => (() -> String) -> 'Maybe'       a -> m a
-- 'hoistFail' :: 'MonadFail' m => (e  -> String) -> 'Either'  e   a -> m a
-- 'hoistFail' :: 'MonadFail' m => (e  -> String) -> 'ExceptT' e m a -> m a
-- @
--
-- @since 0.3.0.0
hoistFail
  :: (PluckError e t m, MonadFail m)
  => (e -> String)
  -> t a
  -> m a
hoistFail f = foldError (fail . f) pure

-- | @hoistFail' = hoistFail id@
--
-- @since 0.3.0.0
hoistFail' :: (PluckError String t m, MonadFail m) => t a -> m a
hoistFail' = hoistFail id

-- | A version of 'hoistFail' that operates on values already in the monad.
--
-- @
-- 'hoistFailM' :: 'MonadFail' m => (() -> String) -> m ('Maybe'       a) -> m a
-- 'hoistFailM' :: 'MonadFail' m => (e  -> String) -> m ('Either'  e   a) -> m a
-- 'hoistFailM' :: 'MonadFail' m => (e  -> String) -> m ('ExceptT' e m a) -> m a
-- @
--
-- @since 0.3.0.0
hoistFailM
  :: (PluckError e t m, MonadFail m)
  => (e -> String)
  -> m (t a)
  -> m a
hoistFailM f m = m >>= hoistFail f

-- | @hoistFailM' = hoistFailM id@
--
-- @since 0.3.0.0
hoistFailM'
  :: (PluckError String t m, MonadFail m)
  => m (t a)
  -> m a
hoistFailM' = hoistFailM id

-- | A flipped synonym for 'hoistFail'. Mnemonic: @#@ looks a bit like @F@.
--
-- @
-- ('<%#>') :: 'MonadFail' m => 'Maybe'       a -> (() -> String) -> m a
-- ('<%#>') :: 'MonadFail' m => 'Either'  e m a -> (e  -> String) -> m a
-- ('<%#>') :: 'MonadFail' m => 'ExceptT' e m a -> (e  -> String) -> m a
-- @
--
-- @since 0.3.0.0
(<%#>)
  :: (PluckError e t m, MonadFail m)
  => t a
  -> (e -> String)
  -> m a
(<%#>) = flip hoistFail

infixl 8 <%#>
{-# INLINE (<%#>) #-}

-- | A flipped synonym for 'hoistFailM'.
--
-- @
-- ('<%!#>') :: 'MonadFail' m => m ('Maybe'       a) -> (() -> String) -> m a
-- ('<%!#>') :: 'MonadFail' m => m ('Either'  e   a) -> (e  -> String) -> m a
-- ('<%!#>') :: 'MonadFail' m => m ('ExceptT' e m a) -> (e  -> String) -> m a
-- @
--
-- @since 0.3.0.0
(<%!#>)
  :: (PluckError e t m, MonadFail m)
  => m (t a)
  -> (e -> String)
  -> m a
(<%!#>) = flip hoistFailM

infixl 8 <%!#>
{-# INLINE (<%!#>) #-}

-- | A version of '<%#>' that ignores the error in @t a@ and fails
-- with a new one.
--
-- @
-- ('<#>') :: 'MonadFail' m => 'Maybe'       a -> String -> m a
-- ('<#>') :: 'MonadFail' m => 'Either'  e   a -> String -> m a
-- ('<#>') :: 'MonadFail' m => 'ExceptT' e m a -> String -> m a
-- @
--
-- @since 0.3.0.0
(<#>)
  :: (PluckError e t m, MonadFail m)
  => t a
  -> String
  -> m a
m <#> e = m <%#> const e

infixl 8 <#>
{-# INLINE (<#>) #-}

-- | A version of '<#>' that operates on values already in the monad.
--
-- @
-- ('<!#>') :: 'MonadFail' m => m ('Maybe'       a) -> String -> m a
-- ('<!#>') :: 'MonadFail' m => m ('Either'  e   a) -> String -> m a
-- ('<!#>') :: 'MonadFail' m => m ('ExceptT' e m a) -> String -> m a
-- @
--
-- @since 0.3.0.0
(<!#>)
  :: (PluckError e t m, MonadFail m)
  => m (t a)
  -> String
  -> m a
m <!#> e = m >>= hoistFail (const e)

infixl 8 <!#>
{-# INLINE (<!#>) #-}

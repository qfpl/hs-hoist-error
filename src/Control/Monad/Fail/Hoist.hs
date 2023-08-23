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
  , hoistFailShow
  , hoistFailString
  , (<%!>)
  , (<!>)
  ) where

import           Control.Monad.Error.Class  (MonadError (..))

import           Data.Either                (Either, either)

class Monad m => HoistFail m t e | t -> e where

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

instance MonadFail m => HoistFail m Maybe () where
  hoistFail f = maybe (fail $ f ()) pure

instance MonadFail m => HoistFail m (Either e) e where
  hoistFail f = either (fail . f) pure

hoistFailShow :: (HoistFail m t e, Show e) => t a -> m a
hoistFailShow = hoistFail show

hoistFailString :: HoistFail m t String => t a -> m a
hoistFailString = hoistFail id

-- | A flipped synonym for 'hoistFail'.
--
-- @
-- ('<%!>') :: 'MonadFail' m => 'Maybe'       a -> (() -> e) ->           m a
-- ('<%!>') :: 'MonadFail' m => 'Either'  a   b -> (a  -> e) ->           m b
-- @
(<%!>)
  :: HoistFail m t e
  => t a
  -> (e -> String)
  -> m a
(<%!>) = flip hoistFail

infixl 8 <%!>
{-# INLINE (<%!>) #-}

-- | A version of '<%!>' that ignores the error in @t a@ and replaces it
-- with a new one.
--
-- @
-- ('<!>') :: 'MonadFail' m => 'Maybe'       a -> e ->           m a
-- ('<!>') :: 'MonadFail' m => 'Either'  a   b -> e ->           m b
-- @
(<!>)
  :: HoistFail m t e
  => t a
  -> String
  -> m a
m <!> e = m <%!> const e

infixl 8 <!>
{-# INLINE (<!>) #-}

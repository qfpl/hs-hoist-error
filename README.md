## Hoist Error

A typeclass and some combinators to aid in the lifting of errors into your preferred context.

As an example, assuming we have the following error type:
```haskell
data MyError
  = WrongInt Int
  | WrapLibError LibError
```

This package provides a combinator for straight forward translation of a general error case into a more specific type:
```haskell
-- Given a function:
fn :: MonadIO m => Int -> m (Maybe b)

-- Working in some MonadError context..
... :: MonadError MyError m => m b

-- Handle the `Nothing` case by using an explicit constructor, with no intermediate boilerplate.
g :: (MonadIO m, MonadError MyError m) => Int -> m b
g n = fn n <!?> WrongInt n
```

You're able to run a function on the error as well:
```haskell
-- Given...
fn :: MonadIO m => Int -> Either LibError b

-- Provide an `(e -> e')` function as required. In this case, wrapping a general
-- library error with our more relevant constructor.
g :: (MonadIO m, MonadError MyError m) => Int -> m b
g n = fn n <%!?> WrapLibError
```

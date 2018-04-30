module Common
    ( onLeftThrow
    , onNothingThrow
    ) where

import Control.Monad.Catch ( Exception, MonadThrow (..), throwM )

onNothingThrow :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
onNothingThrow e = maybe (throwM e) return

onLeftThrow :: (Exception e, MonadThrow m) => (a -> e) -> Either a b -> m b
onLeftThrow toException = either (throwM . toException) return

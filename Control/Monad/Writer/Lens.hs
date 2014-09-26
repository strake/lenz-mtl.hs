module Control.Monad.Writer.Lens where

import Control.Applicative
import Control.Category.Unicode
import Control.Lens as L
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as M

tells :: (MonadWriter β m, Monoid α) => Lens α β a b -> b -> m ()
tells l x = M.tell (L.set l x mempty)

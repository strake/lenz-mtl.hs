module Control.Monad.State.Lens (gets, puts, state, modify, modifyM) where

import Control.Applicative
import Control.Category.Unicode
import Control.Lens (Lens)
import qualified Control.Lens as L
import Control.Monad ((>=>))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as M
import qualified Control.Monad.State.Lazy   as LazyState
import qualified Control.Monad.State.Strict as StrictState

gets :: (MonadState α m) => ((a -> Const a b) -> α -> Const a α) -> m a
gets = M.gets ∘ L.get

puts :: (MonadState α m) => Lens α α a b -> b -> m a
puts l = liftA2 pure (gets l) ∘ M.modify ∘ L.set l

state :: (MonadState α m) => Lens α α a b -> (a -> (c, b)) -> m c
state l f = f <$> gets l >>= \ (x, s) -> x <$ puts l s

modify :: (MonadState α m) => Lens α α a b -> (a -> b) -> m a
modify l = liftA2 pure (gets l) ∘ M.modify ∘ L.modify l

modifyM :: (MonadState α m) => Lens α α a b -> (a -> m b) -> m a
modifyM l f = M.get >>= liftA2 (<$) (L.get l) (l f >=> M.put)

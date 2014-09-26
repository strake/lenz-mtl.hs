module Control.Monad.Reader.Lens where

import Control.Applicative
import Control.Category.Unicode
import Control.Lens as L
import Control.Monad.Reader hiding (asks, local)
import qualified Control.Monad.Reader as M

asks :: (MonadReader α m) => ((a -> Const a b) -> α -> Const a β) -> m a
asks = M.asks ∘ L.get

local :: (MonadReader α m) => Lens α α a b -> (a -> b) -> m c -> m c
local = M.local ∘∘ L.modify where f ∘∘ g = (f ∘) ∘ g

localM :: (MonadReader α m) => Lens α α a b -> (a -> m b) -> m c -> m c
localM l f x = M.ask >>= l f >>= flip M.local x ∘ pure

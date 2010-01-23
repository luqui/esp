module ESP.Command where

import Control.Applicative
import ESP.UI
import Data.Monoid (Monoid(..))
import Control.Monad (ap, (>=>))

data Command i o a 
    = Done a
    | More o (i -> Command i o a)

instance Functor (Command i o) where
    fmap f (Done x) = Done (f x)
    fmap f (More o c) = More o ((fmap.fmap) f c)

instance Monad (Command i o) where
    return = Done
    Done x >>= f = f x
    More o c >>= f = More o (c >=> f)

instance Applicative (Command i o) where
    pure = return
    (<*>) = ap

instance Monoid o => Monoid (Command i o a) where
    Done x `mappend` _ = Done x
    _ `mappend` Done x = Done x
    More o c `mappend` More o' c' = More (o `mappend` o') (liftA2 mappend c c')

mapCommand :: (o -> o') -> Command i o a -> Command i o' a
mapCommand f (Done x) = Done x
mapCommand f (More o c) = More (f o) ((fmap.mapCommand) f c)

runCommand :: (a -> UI i o) -> Command i o a -> UI i o
runCommand f (Done x) = f x
runCommand f (More o c) = UI o (runCommand f . c)

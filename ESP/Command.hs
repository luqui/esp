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

mapCommand :: (o -> o') -> Command i o a -> Command i o' a
mapCommand f (Done x) = Done x
mapCommand f (More o c) = More (f o) ((fmap.mapCommand) f c)

zipCommand :: (o -> o' -> o'') -> Command i o a -> Command i o' a -> Command i o'' a
zipCommand f (Done a) _ = Done a
zipCommand f _ (Done a) = Done a
zipCommand f (More o c) (More o' c') = More (f o o') (liftA2 (zipCommand f) c c')

putback :: i -> Command i o a -> Command i o a
putback x (Done a) = Done a
putback x (More o c) = c x

runCommand :: (a -> UI i o) -> Command i o a -> UI i o
runCommand f (Done x) = f x
runCommand f (More o c) = UI o (runCommand f . c)

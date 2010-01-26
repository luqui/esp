module ESP.Command (Command, input, mapCommand, putBack, runCommand, repeatCommand) where

import Control.Applicative
import ESP.UI
import Control.Monad (ap, (>=>), forever)

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

input :: o -> Command i o i
input o = More o Done

mapCommand :: (o -> o') -> Command i o a -> Command i o' a
mapCommand f (Done x) = Done x
mapCommand f (More o c) = More (f o) ((fmap.mapCommand) f c)


putBack :: i -> Command i o a -> Command i o a
putBack x (Done a) = Done a
putBack x (More o c) = c x

runCommand :: (a -> UI i o) -> Command i o a -> UI i o
runCommand f (Done x) = f x
runCommand f (More o c) = display o (runCommand f . c)

repeatCommand :: Command i o a -> UI i o
repeatCommand = runCommand undefined . forever

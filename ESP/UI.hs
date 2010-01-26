module ESP.UI (UI, cofmapUI, display, UserInput(..), runVty) where

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative

data UI a b = UI b (a -> UI a b)

instance Functor (UI a) where
    fmap f (UI b t) = UI (f b) ((fmap.fmap) f t)

instance Applicative (UI a) where
    pure x = go where go = UI x (const go)
    UI f0 ft <*> UI x0 xt = UI (f0 x0) (liftA2 (<*>) ft xt)

cofmapUI :: (a -> b) -> UI b c -> UI a c
cofmapUI f (UI x0 xt) = UI x0 (cofmapUI f . xt . f)

display :: b -> (a -> UI a b) -> UI a b
display = UI


data UserInput = UserInput W.Key [W.Modifier]

runVty :: UI UserInput W.AnyWidget -> IO ()
runVty ui = do
    vty <- W.mkVty
    let mainLoop (UI ui0 uit) = do
            pic <- W.pic_for_image <$> W.mkImage vty ui0
            W.update vty pic
            event <- W.next_event vty
            case event of
                W.EvKey W.KEsc _ -> return ()
                W.EvKey key mods -> mainLoop (uit (UserInput key mods))
                _ -> return ()
    mainLoop ui
    W.shutdown vty

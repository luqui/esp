module ESP.WidgetUtils where

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W

(<++>) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <++> y = W.anyWidget (x W.<++> y)

(<-->) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <--> y = W.anyWidget (x W.<--> y)

textCursor :: String -> W.AnyWidget
textCursor s = text defaultAttr s <++> text highlightAttr " " <++> text defaultAttr " "

text :: W.Attr -> String -> W.AnyWidget
text attr = W.anyWidget . W.text attr

defaultAttr = W.Attr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.white, W.back_color = W.SetTo W.black }

highlightAttr = defaultAttr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.black, W.back_color = W.SetTo W.white }

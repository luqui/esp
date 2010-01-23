import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative
import Data.Monoid (Monoid(..))
import ESP.UI
import ESP.Command
import ESP.Parser

keepAttr = W.Attr { W.style = W.KeepCurrent, W.fore_color = W.KeepCurrent, W.back_color = W.KeepCurrent }
highlightAttr = keepAttr { W.style = W.SetTo 4 }


editText :: String -> Command UserInput String String
editText = go
    where
    go cur = More cur $ \(UserInput key _) ->
        case key of
            W.KASCII ch -> go (cur ++ [ch])
            W.KBS -> go (safeInit cur)
            W.KEnter -> Done cur
            _ -> go cur
    safeInit xs | null xs = []
                | otherwise = init xs

textEditor :: String -> UI UserInput W.AnyWidget
textEditor text0 = UI (text highlightAttr text0) $ \(UserInput key _) ->
    case key of
        W.KASCII 'e' -> runCommand textEditor (mapCommand (text keepAttr) (editText text0))
        W.KASCII 'c' -> runCommand textEditor (mapCommand (text keepAttr) (editText ""))
        _ -> textEditor text0

text :: W.Attr -> String -> W.AnyWidget
text attr = W.anyWidget . W.text attr
 

main = runVty (textEditor "")
